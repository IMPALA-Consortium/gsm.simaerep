HTMLWidgets.widget({
    name: 'Widget_Simaerep',
    type: 'output',
    factory: function(el, width, height) {
        return {
            renderValue: function(x) {
                // Debug logging if enabled
                if (x.config && x.config.bDebug) {
                    console.log("Widget_Simaerep data:", x);
                }

                // Extract data and config from x
                const data = x.data || {};
                const config = x.config || {};

                // Helper function to convert R data frame (columns format) to array of objects (rows format)
                function convertRDataFrame(df) {
                    if (!df || typeof df !== 'object') return [];

                    // Check if it's already in rows format (array of objects)
                    if (Array.isArray(df)) return df;

                    // Convert from columns format to rows format
                    const keys = Object.keys(df);
                    if (keys.length === 0) return [];

                    const firstKey = keys[0];
                    if (!Array.isArray(df[firstKey])) {
                        // Already a single object, wrap in array
                        return [df];
                    }

                    const numRows = df[firstKey].length;
                    const rows = [];

                    for (let i = 0; i < numRows; i++) {
                        const row = {};
                        keys.forEach(key => {
                            row[key] = df[key][i];
                        });
                        rows.push(row);
                    }

                    return rows;
                }

                // Convert R data frames to JavaScript arrays
                console.log("Converting R data frames to JavaScript format...");
                const chartData = {
                    df_mean_study: convertRDataFrame(data.df_mean_study),
                    df_mean_group_flagged: convertRDataFrame(data.df_mean_group_flagged),
                    df_mean_group_not_flagged: convertRDataFrame(data.df_mean_group_not_flagged),
                    df_label_sites: convertRDataFrame(data.df_label_sites)
                };

                // Log what we received after conversion
                console.log("Rendering Simaerep widget...");
                console.log("- df_mean_study points:", chartData.df_mean_study.length);
                console.log("- df_mean_group_flagged points:", chartData.df_mean_group_flagged.length);
                console.log("- df_mean_group_not_flagged points:", chartData.df_mean_group_not_flagged.length);
                console.log("- df_label_sites:", chartData.df_label_sites.length);

                // Assign unique ID to element
                el.id = `simaerep--${config.strStudyId}_${config.strScoreCol}`;

                // Configure chart
                const chartConfig = {
                    selectedGroupIDs: config.selectedGroupIDs || 'None',
                    aspectRatio: config.aspectRatio || 2,
                    showGroupSelector: config.showGroupSelector !== false,
                    maxHeight: config.maxHeight || '600px',
                    width: width,
                    height: height,
                    groupLabelKey: config.groupLabelKey || 'GroupID',
                    groupMetadata: convertRDataFrame(config.dfGroups),
                    metric: config.lMetric
                };

                // Check if gsmSimaerepViz is available
                if (typeof gsmSimaerepViz === 'undefined') {
                    console.error("gsmSimaerepViz is not defined! Check if index.js loaded.");
                    el.innerHTML = '<div style="color: red; padding: 20px;">Error: gsmSimaerepViz library not loaded</div>';
                    return;
                }

                // Check if Simaerep class exists
                if (typeof gsmSimaerepViz.Simaerep === 'undefined') {
                    console.error("gsmSimaerepViz.Simaerep is not defined! Available:", Object.keys(gsmSimaerepViz));
                    el.innerHTML = '<div style="color: red; padding: 20px;">Error: Simaerep class not found. Available: ' + Object.keys(gsmSimaerepViz).join(', ') + '</div>';
                    return;
                }

                // Create Simaerep instance
                console.log("Creating Simaerep instance...");
                try {
                    const instance = new gsmSimaerepViz.Simaerep(
                        el,
                        chartData,
                        chartConfig
                    );

                    console.log("Simaerep instance created successfully!");

                    // Store instance for later access
                    el.chartInstance = instance;
                } catch (error) {
                    console.error("Error creating Simaerep instance:", error);
                    console.error("Stack:", error.stack);
                    el.innerHTML = '<div style="color: red; padding: 20px;"><strong>Error creating chart:</strong><br>' + error.message + '<br><br><pre>' + error.stack + '</pre></div>';
                }
            },
            resize: function(width, height) {
                if (el.chartInstance) {
                    el.chartInstance.data.config.width = width;
                    el.chartInstance.data.config.height = height;
                    el.chartInstance.render();
                }
            }
        };
    }
});
