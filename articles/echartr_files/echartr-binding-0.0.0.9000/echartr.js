HTMLWidgets.widget({
  name: 'echartr',
  type: 'output',

  factory: function(el, width, height) {

    var chart;
    var initialised = false;

    return {

      renderValue: function(x) {

        if (!initialised) {
          initialised = true;

          if (HTMLWidgets.shinyMode) {
            Shiny.addCustomMessageHandler("__echartr__".concat(el.id), x => {
              update_echartr(chart, x)
            });
          }

        } else if (!x.update) {
          chart.dispose();
        }

        chart = echarts.init(el);
        update_echartr(chart, x);

      },

      resize: function(width, height) {
      },

    };
  }
});

function update_echartr(chart, event) {
  if (event.option !== null) {
    chart.setOption(event.option);
  }
  if (event.on !== null) {
    event.on.forEach(x => {
      // TODO: More concise way?
      if (Object.hasOwn(x, "query")) {
        chart.on(x.eventName, x.query, eval(x.handler), context = chart);
      } else {
        chart.on(x.eventName, eval(x.handler), context = chart);
      }
    });
  }
  if (event.off !== null) {
    // TODO: Support specifying handler
    event.off.forEach(x => {
      chart.off(x);
    });
  }
  if (event.dispatch !== null) {
    event.dispatch.forEach(x => {
      chart.dispatchAction(x);
    });
  }
  if (HTMLWidgets.shinyMode && event.listen != null) {
    event.listen.forEach(x => {
      chart.on(x, e => {
        const shinyInput = chart._dom.id + "_" + x;
        delete e.event; // Mouse events have an event property that is not readily serializable
        Shiny.setInputValue(shinyInput, e);
      })
    })
  }
}




