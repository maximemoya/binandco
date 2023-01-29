package fr.maxime.binandco
package tools.swing

import java.awt.*
import java.awt.event.ActionListener
import java.io.{File, IOException, PrintWriter}
import java.time.Instant
import java.util.{Date, UUID}
import javax.imageio.ImageIO
import javax.swing.{ImageIcon, JButton, JFrame, JLabel, JPanel, JTextArea, Timer, WindowConstants}
import scala.util.Random

object Timer {
  def apply(interval: Int, repeats: Boolean = true)(op: => Unit): Unit = {
    val timeOut = new javax.swing.AbstractAction() {
      def actionPerformed(e: java.awt.event.ActionEvent): Unit = op
    }
    val t = new javax.swing.Timer(interval, timeOut)
    t.setRepeats(repeats)
    t.start()
  }
}

class MainSwing {

  val frame = new JFrame("JFrame Example")
  val panel = new JPanel()
  panel.setLayout(new FlowLayout())
  val label = new JLabel("0")
  label.setFont(new Font("",0,20))
  val rand = new Random()
  Timer(1000) {
    label.setText(String.format("%12s",rand.nextInt().abs).replace(" ", "0"))
  }

  //  val button = new JButton()
//  button.setText("Button")

  panel.add(label)
//  panel.add(button)
  frame.add(panel)

  frame.setSize(300, 200)
  frame.setLocationRelativeTo(null)
  frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
  frame.setVisible(true)

}
