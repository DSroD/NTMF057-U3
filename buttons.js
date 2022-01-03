function setBarVisible() {
    Plotly.plot('errorPlotDiv',
        [], {}, { displayModeBar: true }
    );
    Plotly.update('errorPlotDiv',
        {}, {
        xaxis: {
            type: 'log'
        },
        yaxis: {
            type: 'log'
        }
    });
}

window.addEventListener('load', setBarVisible);