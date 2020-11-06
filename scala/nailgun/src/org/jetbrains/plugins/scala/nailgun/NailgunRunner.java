package org.jetbrains.plugins.scala.nailgun;

import com.martiansoftware.nailgun.Alias;
import com.martiansoftware.nailgun.NGServer;

import java.io.File;
import java.io.IOException;
import java.net.InetAddress;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.PosixFileAttributeView;
import java.nio.file.attribute.PosixFilePermission;
import java.util.HashSet;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Stream;

import static java.util.Arrays.asList;

/**
 * used from {@link org.jetbrains.plugins.scala.compiler.CompileServerLauncher}
 * @author Pavel Fatin
 */
@SuppressWarnings("JavadocReference")
public class NailgunRunner {
  public static final String SERVER_CLASS_NAME = "org.jetbrains.jps.incremental.scala.remote.Main";

  /** NOTE: set of commands should be equal to commands from {@link org.jetbrains.jps.incremental.scala.remote.CommandIds} */
  private static final String[] COMMANDS = {
          "compile",
          "compile-jps",
          "get-metrics",
          "start-metering",
          "end-metering"
  };
  private static final String SERVER_DESCRIPTION = "Scala compile server";

  private static final String STOP_ALIAS_START = "stop_";
  private static final String STOP_CLASS_NAME = "com.martiansoftware.nailgun.builtins.NGStop";

  public static void main(String[] args) throws IOException, ClassNotFoundException {
    if (args.length != 3) throw new IllegalArgumentException("Usage: NailgunRunner [port] [id] [classpath]");

    int port = Integer.parseInt(args[0]);
    String id = args[1];
    String classpath = args[2];

    URLClassLoader classLoader = constructClassLoader(classpath);

    TokensGenerator.generateAndWriteTokenFor(port);

    InetAddress address = InetAddress.getByName(null);
    NGServer server = createServer(address, port, id, classLoader);

    Thread thread = new Thread(server);
    thread.setName("NGServer(" + address.toString() + ", " + port + "," + id + ")");
    thread.setContextClassLoader(classLoader);
    thread.start();

    Runtime.getRuntime().addShutdownHook(new ShutdownHook(server));
  }

  /**
   * Extra Uclass loader is required because dotty compiler interfaces (used inside Main during compilation)
   * casts classloader to a URLClassloader, and in JRE 11 AppClassLoader is not an instance of URLClassloader.
   */
  public static URLClassLoader constructClassLoader(String classpath) {
    Function<String, URL> pathToUrl = path -> {
      try {
        return new File(path).toURI().toURL();
      } catch (MalformedURLException e) {
        throw new RuntimeException(e);
      }
    };

    URL[] urls = Stream.of(classpath.split(File.pathSeparator))
            .map(pathToUrl)
            .toArray(URL[]::new);
    return new URLClassLoader(urls, NailgunRunner.class.getClassLoader());
  }

  private static NGServer createServer(InetAddress address, int port, String id, URLClassLoader classLoader)
      throws ClassNotFoundException {
    NGServer server = new NGServer(address, port);

    server.setAllowNailsByClassName(false);

    Class<?> serverClass = classLoader.loadClass(SERVER_CLASS_NAME);
    for (String command : COMMANDS) {
      server.getAliasManager().addAlias(new Alias(command, SERVER_DESCRIPTION, serverClass));
    }

    // TODO: token should be checked
    Class<?> stopClass = classLoader.loadClass(STOP_CLASS_NAME);
    String stopAlias = STOP_ALIAS_START + id;
    server.getAliasManager().addAlias(new Alias(stopAlias, "", stopClass));

    return server;
  }

  private static class ShutdownHook extends Thread {
    static final int TIMEOUT = 30;

    private final NGServer myServer;

    ShutdownHook(NGServer server) {
      myServer = server;
    }

    public void run() {
      TokensGenerator.deleteTokenFor(myServer.getPort());

      myServer.shutdown(false);

      for (int i = 0; i < TIMEOUT; i++) {
        if (!myServer.isRunning()) break;

        try {
          Thread.sleep(100);
        } catch (InterruptedException e) {
          // do nothing
        }
      }
    }
  }

  private static class TokensGenerator {

    static void generateAndWriteTokenFor(int port) throws IOException {
      Path path = tokenPathFor(port);
      writeTokenTo(path, UUID.randomUUID());
    }

    /** duplicated in {@link org.jetbrains.plugins.scala.server.CompileServerToken} */
    static Path tokenPathFor(int port) {
      return Paths.get(System.getProperty("user.home"), ".idea-build", "tokens", Integer.toString(port));
    }

    static void writeTokenTo(Path path, UUID uuid) throws IOException {
      File directory = path.getParent().toFile();

      if (!directory.exists()) {
        if (!directory.mkdirs()) {
          throw new IOException("Cannot create directory: " + directory);
        }
      }

      Files.write(path, uuid.toString().getBytes());

      PosixFileAttributeView view = Files.getFileAttributeView(path, PosixFileAttributeView.class);
      if (view != null) {
        try {
          view.setPermissions(new HashSet<>(asList(PosixFilePermission.OWNER_READ, PosixFilePermission.OWNER_WRITE)));
        } catch (IOException e) {
          System.err.println("Cannot set permissions: " + path);
        }
      }
    }

    public static void deleteTokenFor(int port) {
      File tokenFile = tokenPathFor(port).toFile();
      if (!tokenFile.delete()) {
        tokenFile.deleteOnExit();
      }
    }
  }
}
