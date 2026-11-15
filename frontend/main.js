import './style.css'
import './src/editor.css'
import { createEditor } from './src/editor.js'
import { startHighlight } from './src/highlight.js'
import { squareThumb } from './src/image.js'
import { startLinkTitles } from './src/links.js'

// Scala.js の画面から使うエディタ（Tiptap）
window.skeaEditor = { create: createEditor }
window.skeaImage = { squareThumb }
window.skeaLinks = { start: startLinkTitles }
startHighlight()

import 'scalajs:main.js'
