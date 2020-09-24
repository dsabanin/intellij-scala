package org.jetbrains.plugins.scala.dfa.utils

import scala.collection.mutable

class IndexMap[T] extends mutable.Map[Int, T] {
  private val buffer = mutable.ArrayBuffer.empty[Option[T]]
  private var _size = 0

  override def iterator: Iterator[(Int, T)] =
    Iterator.from(0).zip(buffer.iterator.flatten)

  override def subtractOne(key: Int): IndexMap.this.type = {
    if (key < buffer.size) {
      if (buffer(key).isDefined) {
        _size -= 1
      }
      buffer(key) = None
    }
    this
  }

  override def addOne(elem: (Int, T)): IndexMap.this.type = {
    val (key, value) = elem
    if (key < buffer.size) {
      if (buffer(key).isEmpty) {
        _size += 1
      }
      buffer(key) = Some(value)
    } else {
      while (buffer.size < key) {
        buffer += None
      }
      buffer += Some(value)
      _size += 1
    }

    this
  }

  override def get(key: Int): Option[T] =
    if (key < buffer.size) buffer(key) else None

  override def isEmpty: Boolean = _size == 0

  override def knownSize: Int = size

  override def size: Int = _size
}

object IndexMap {
  def empty[T]: IndexMap[T] = new IndexMap[T]
}