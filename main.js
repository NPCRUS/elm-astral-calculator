// Modules to control application life and create native browser window
const {app, BrowserWindow, Menu, dialog, webContents} = require('electron')
const path = require('path')
const fs = require('fs')

const template = [
    { label: "File", submenu: [
        { label: 'Save As', click: saveDialog},
        { role: 'quit', label: 'Exit'}
    ]}
]

const pdfOptions = {
  marginsType: 0,
  printBackground: false,
  printSelectionOnly: false,
  landscape: false,
  pageSize: 'A4',
  scaleFactor: 50
}

function addOnlyPrintNotDisplayed(content) {
  content.insertCSS('.only-print {display: none !important}')
}

async function saveDialog() {
  try {
    const result = await dialog.showSaveDialog({
        defaultPath: 'synastry',
        filters: [{
          name: 'Adobe PDF',
          extensions: ['pdf']
        }]
    })

    const content = BrowserWindow.getFocusedWindow().webContents
    if(!result.canceled) {
        const cssIds = await Promise.all([
          content.insertCSS('#no-print {display: none !important}'),
          content.insertCSS('.only-print {display: block !important}')
        ])
        const data = await content.printToPDF({})
        cssIds.map(id => content.removeInsertedCSS(id))
        await writeFile(data, result.filePath)
    }
  } catch (ex) {
    dialog.showMessageBox({ title: "Error", message: ex.toString()})
  } 
}

function writeFile(data, filePath) {
    return new Promise((resolve, reject) => {
        fs.writeFile(filePath, data, error => {
            if(error) reject(error)
            else resolve(null)
        })
    })
}

function createWindow () {
  // Create the browser window.
  const mainWindow = new BrowserWindow({
    width: 818,
    height: 1000,
    webPreferences: {
      preload: path.join(__dirname, 'preload.js')
    }
  })

  // and load the index.html of the app.
  mainWindow.loadFile('index.html')

  // Open the DevTools.
  // mainWindow.webContents.openDevTools()
}

// This method will be called when Electron has finished
// initialization and is ready to create browser windows.
// Some APIs can only be used after this event occurs.
app.whenReady().then(() => {
  createWindow()

  app.on('activate', function () {
    // On macOS it's common to re-create a window in the app when the
    // dock icon is clicked and there are no other windows open.
    if (BrowserWindow.getAllWindows().length === 0) createWindow()
  })
})

// Quit when all windows are closed, except on macOS. There, it's common
// for applications and their menu bar to stay active until the user quits
// explicitly with Cmd + Q.
app.on('window-all-closed', function () {
  if (process.platform !== 'darwin') app.quit()
})

// In this file you can include the rest of your app's specific main process
// code. You can also put them in separate files and require them here.

Menu.setApplicationMenu(Menu.buildFromTemplate(template))