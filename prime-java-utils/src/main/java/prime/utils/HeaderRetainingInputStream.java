package prime.utils;

import java.io.FilterInputStream;
import java.io.InputStream;
import java.io.IOException;
import java.util.Arrays;


/**
 * A FilterInputStream implementation that retains the first number of
 * bytes, while they are being read. It also keeps track of the total
 * number of bytes read.
 *
 * By Arnout Roemers.
 */
public class HeaderRetainingInputStream extends FilterInputStream {

  /** The retained bytes. */
  private byte[] header;

  /** The number of bytes read. */
  private int readBytes;

  /** An indicator of recursion. */
  private boolean reading;

  /** An optimisation to stop trying to fill the header array when its full. */
  private boolean done;


  /**
   * Create a new HeaderRetainingInputStream.
   * @param in The InputStream to wrap.
   * @param retain The number of bytes to retain.
   */
  public HeaderRetainingInputStream(final InputStream in, final int retain) {
    super(in);
    this.header = new byte[retain];
    this.reading = false;
    this.done = false;
  }


  // Extra functions.

  /**
   * Returns the number of bytes read.
   */
  public int bytesRead() {
    return this.readBytes;
  }

  /**
   * Returns the byte-array of the retained first bytes. The size of
   * this array may be smaller than the requested number of bytes to
   * retain. This may be because of the size of the data or because
   * not all bytes have been read.
   */
  public byte[] header() {
    return Arrays.copyOf(this.header, Math.min(this.readBytes, this.header.length));
  }


  // Overriding functions.

  public int read() throws IOException {
    this.reading = true;
    final int b = in.read();
    this.reading = false;
    if (!done && !reading && b != -1) {
      this.header[this.readBytes] = (byte)b;
      this.readBytes++;
      this.done = this.readBytes >= this.header.length;
    }
    return b;
  }


  public int read(final byte[] b, final int off, final int len) throws IOException {
    this.reading = true;
    final int r = in.read(b, off, len);
    this.reading = false;
    if (!done) {
      for (int i = 0; i < r; i++) {
        if (this.readBytes < this.header.length)
          this.header[this.readBytes] = b[i];
        this.readBytes++;
      }
      this.done = this.readBytes >= this.header.length;
    } else
      readBytes += r;
    return r;
  }

}
