// todo(kevan): look over this and make sense of it.
// Should be a fun exercise.

override def prepended[B >: A](value: B): Vector[B] = {
  if (endIndex != startIndex) {
    val blockIndex = (startIndex - 1) & ~31
    val lo = (startIndex - 1) & 31

    if (startIndex != blockIndex + 32) {
      val s = new Vector(startIndex - 1, endIndex, blockIndex)
      s.initFrom(this)
      s.dirty = dirty
      s.gotoPosWritable(focus, blockIndex, focus ^ blockIndex)
      s.display0(lo) = value.asInstanceOf[AnyRef]
      s
    } else {

      val freeSpace = (1 << (5 * depth)) - endIndex           // free space at the right given the current tree-structure depth
      val shift = freeSpace & ~((1 << (5 * (depth - 1))) - 1) // number of elements by which we'll shift right (only move at top level)
      val shiftBlocks = freeSpace >>> (5 * (depth - 1))       // number of top-level blocks

      if (shift != 0) {
        // case A: we can shift right on the top level
        if (depth > 1) {
          val newBlockIndex = blockIndex + shift
          val newFocus = focus + shift

          val s = new Vector(startIndex - 1 + shift, endIndex + shift, newBlockIndex)
          s.initFrom(this)
          s.dirty = dirty
          s.shiftTopLevel(0, shiftBlocks) // shift right by n blocks
          s.gotoFreshPosWritable(newFocus, newBlockIndex, newFocus ^ newBlockIndex) // maybe create pos; prepare for writing
          s.display0(lo) = value.asInstanceOf[AnyRef]
          s
        } else {
          val newBlockIndex = blockIndex + 32
          val newFocus = focus

          val s = new Vector(startIndex - 1 + shift, endIndex + shift, newBlockIndex)
          s.initFrom(this)
          s.dirty = dirty
          s.shiftTopLevel(0, shiftBlocks) // shift right by n elements
          s.gotoPosWritable(newFocus, newBlockIndex, newFocus ^ newBlockIndex) // prepare for writing
          s.display0(shift - 1) = value.asInstanceOf[AnyRef]
          s
        }
      } else if (blockIndex < 0) {
        // case B: we need to move the whole structure
        val move = (1 << (5 * (depth + 1))) - (1 << (5 * depth))
        val newBlockIndex = blockIndex + move
        val newFocus = focus + move

        val s = new Vector(startIndex - 1 + move, endIndex + move, newBlockIndex)
        s.initFrom(this)
        s.dirty = dirty
        s.gotoFreshPosWritable(newFocus, newBlockIndex, newFocus ^ newBlockIndex) // could optimize: we know it will create a whole branch
        s.display0(lo) = value.asInstanceOf[AnyRef]
        s
      } else {
        val newBlockIndex = blockIndex
        val newFocus = focus

        val s = new Vector(startIndex - 1, endIndex, newBlockIndex)
        s.initFrom(this)
        s.dirty = dirty
        s.gotoFreshPosWritable(newFocus, newBlockIndex, newFocus ^ newBlockIndex)
        s.display0(lo) = value.asInstanceOf[AnyRef]
        s
      }
    }
  } else {
    // empty vector, just insert single element at the back
    val elems = new Array[AnyRef](32)
    elems(31) = value.asInstanceOf[AnyRef]
    val s = new Vector(31, 32, 0)
    s.depth = 1
    s.display0 = elems
    s
  }
}
