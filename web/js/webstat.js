require.config({
  urlArgs: "ts="+new Date().getTime(),	/* prevent caching during development */
  waitSeconds: 60,			/* webstat-min.js is big */
  paths:
  { tabulator:		"../node_modules/tabulator-tables/dist/js/tabulator.min",
    jquery:             "../node_modules/jquery/dist/jquery.min",
    laconic:            "../node_modules/laconic/laconic",
    bootstrap:          "../node_modules/bootstrap/dist/js/bootstrap.min",
    "svg-pan-zoom":     "../node_modules/svg-pan-zoom/dist/svg-pan-zoom.min",
    sparkline:	        "../node_modules/sparkline/dist/jquery.sparkline",
    flot:		"../node_modules/jquery.flot.current/jquery.flot",
    palette:		"../node_modules/google-palette/palette"
  },
  shim:
  { bootstrap:
    { deps:["jquery"]
    },
    laconic:
    { deps:["jquery"]
    },
    tagmanager:
    { deps:["jquery"]
    },
  }
}); //require.config

/*
 * Create the webstat application.
 */
require(["jquery", "jwebstat"],
	function($, webstat) {

  $("body").webstat({});
});
