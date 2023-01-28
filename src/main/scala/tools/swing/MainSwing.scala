package fr.maxime.binandco
package tools.swing

import java.awt.FlowLayout
import java.awt.event.ActionListener
import javax.swing.*

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
  val label = new JLabel("000000000.0")
  val button = new JButton()
  Timer(1000){label.setText((Math.random()*100_000_000).toInt.toString)}

  button.setText("Button")
  panel.add(label)
  panel.add(button)
  frame.add(panel)
  frame.setSize(200, 300)
  frame.setLocationRelativeTo(null)
  frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
  frame.setVisible(true)

}
