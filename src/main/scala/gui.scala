package gui

import swing._
import event._
import micro16.State
import micro16.Micro16Simulator

object Micro16SimulatorGUI extends SwingApplication {
	

	override def startup(args: Array[String]) {
		val src = if(args.length > 0) args(0) else "micro16-code.txt"
			
		new MainFrame {
			title = "Micro16 Simulator"
			contents = new BoxPanel(Orientation.Horizontal) {
				val runButton = new Button { text = "Run" }
				val loadButton = new Button { text = "Load code" }
				val stepButton = new Button { text = "Step "}
				val reloadButton = new Button { text = "Reload code" }
				val textArea =  new TextArea(io.Source.fromFile(src).mkString)
				val lineNumber = new ListView( (1 to textArea.text.lines.length + 1).map("%02d \n".format(_))) {
					fixedCellHeight = 15
					font = new Font("Droid Sans Mono Dotted", 0, 12)
				}
				contents += new BoxPanel(Orientation.Vertical) {
					contents += new TabbedPane() {
						pages += new TabbedPane.Page(src,
							new ScrollPane(
								new BoxPanel(Orientation.Horizontal) {				
									textArea.font = lineNumber.font
									contents += lineNumber
									contents += textArea
								}
							)
						)	
					}
					contents += new BoxPanel(Orientation.Horizontal) {
						contents += reloadButton
						contents += loadButton
						contents += runButton
						contents += stepButton

					}
				}

				listenTo(reloadButton)
				listenTo(runButton)
				listenTo(loadButton)
				listenTo(stepButton)

				val state = new ListView(State.dump().split('\n')) {
					font = new Font("Droid Sans Mono Dotted", 0, 12)
				}
				contents += new ScrollPane(state) {
					maximumSize = new Dimension(270, 4000)
					minimumSize = new Dimension(270, 0)
				}

				reactions += {
					case ButtonClicked(`loadButton`) => 
						Micro16Simulator.loadCode(textArea.text)
						state.listData = State.dump().split('\n')
						lineNumber.selectIndices( 0 )
					case ButtonClicked(`runButton`) =>
						Micro16Simulator.loadCode(textArea.text)
						Micro16Simulator.simulate()
						state.listData = State.dump().split('\n')
						lineNumber.selectIndices( State.execPointer )
					case ButtonClicked(`stepButton`) =>
						if(Micro16Simulator.canStep) {
							lineNumber.selectIndices( Micro16Simulator.doStep() )
							state.listData = State.dump().split('\n')
						}
					case ButtonClicked(`reloadButton`) =>
						textArea.text = io.Source.fromFile(src).mkString
						Micro16Simulator.loadCode(textArea.text)
						state.listData = State.dump().split('\n')
						lineNumber.selectIndices( 0 )
					}
			}
		}.open()
	}
}