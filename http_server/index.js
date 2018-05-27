var express = require('express')
var app = express()
const { exec } = require('child_process')

app.get('/estimation', function (req, res) {

    const estimate = exec('pwd');

    estimate.stdout.on('data', data => {
        console.log("Shell returned:\n " + data);
        res.json({
            result: data
        })
    })

})

app.listen(3000)