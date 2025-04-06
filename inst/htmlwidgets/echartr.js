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

        }
        chart = echarts.init(el);
        chart.setOption(x.option);
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
        chart.on(x.eventName, x.query, eval(x.handler));
      } else {
        chart.on(x.eventName, eval(x.handler));
      }
    });
  }
  if (event.off !== null) {
    event.off.forEach(x => {
      chart.off(x.eventName);
    });
  }
}




