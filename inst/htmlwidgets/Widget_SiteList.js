/**
 * htmlwidget binding for SiteList chart - DEBUG VERSION
 */

console.log('[SiteList] Widget binding loading...');

HTMLWidgets.widget({
  name: 'Widget_SiteList',
  type: 'output',
  
  factory: function(el, width, height) {
    console.log('[SiteList] Factory called', {
      element: el,
      width: width,
      height: height,
      elementId: el.id
    });
    
    return {
      renderValue: function(x) {
        console.log('[SiteList] renderValue called');
        console.log('[SiteList] Data received:', x);
        console.log('[SiteList] Data array length:', x.data ? x.data.length : 'NO DATA');
        console.log('[SiteList] Config:', x.config);
        
        // Check if gsmSimaerepViz is available
        console.log('[SiteList] gsmSimaerepViz available:', typeof gsmSimaerepViz);
        console.log('[SiteList] gsmSimaerepViz.SiteList available:', typeof gsmSimaerepViz?.SiteList);
        
        // Add required CSS classes
        console.log('[SiteList] Adding CSS classes to element');
        el.classList.add('gsm-widget', 'site-list');
        console.log('[SiteList] Element classes:', el.className);
        
        // Clear any existing content
        console.log('[SiteList] Clearing element innerHTML');
        el.innerHTML = '';
        
        // Check if we have data
        if (!x.data || !Array.isArray(x.data) || x.data.length === 0) {
          console.error('[SiteList] ERROR: No valid data provided', x.data);
          el.innerHTML = '<div style="color: red; padding: 20px;">Error: No data provided to SiteList widget</div>';
          return;
        }
        
        // Check if gsmSimaerepViz.SiteList exists
        if (typeof gsmSimaerepViz === 'undefined' || typeof gsmSimaerepViz.SiteList !== 'function') {
          console.error('[SiteList] ERROR: gsmSimaerepViz.SiteList not found');
          el.innerHTML = '<div style="color: red; padding: 20px;">Error: gsmSimaerepViz.SiteList library not loaded</div>';
          return;
        }
        
        // Create chart instance
        console.log('[SiteList] Creating SiteList instance...');
        try {
          const chart = new gsmSimaerepViz.SiteList(el, x.data, x.config);
          console.log('[SiteList] Chart created successfully:', chart);
          
          // CRITICAL: Attach chart to canvas for global site selector
          const canvas = el.querySelector('canvas');
          console.log('[SiteList] Canvas element found:', !!canvas);
          
          if (canvas) {
            canvas.chart = chart;
            console.log('[SiteList] Chart attached to canvas');
          } else {
            console.warn('[SiteList] WARNING: No canvas element found in container');
          }
          
          // Create group selector dropdown
          console.log('[SiteList] Creating group selector...');
          const select = document.createElement('select');
          select.className = 'gsm-widget-control--group';
          select.innerHTML = '<option>None</option>';
          
          // Populate dropdown with unique group IDs
          const groupIDs = [...new Set(x.data.map(d => d.GroupID))];
          console.log('[SiteList] Found GroupIDs:', groupIDs);
          
          groupIDs.forEach(id => {
            const option = document.createElement('option');
            option.value = id;
            option.textContent = id;
            if (id === x.config.selectedGroupIDs) {
              option.selected = true;
            }
            select.appendChild(option);
          });
          
          el.appendChild(select);
          console.log('[SiteList] Selector added to DOM');
          
          // Handle local selection changes
          select.addEventListener('change', function(e) {
            console.log('[SiteList] Selection changed to:', e.target.value);
            chart.helpers.updateSelectedGroupIDs(e.target.value);
          });
          
          console.log('[SiteList] Render complete');
          console.log('[SiteList] Final element HTML length:', el.innerHTML.length);
          console.log('[SiteList] Element children:', el.children.length);
          
        } catch (error) {
          console.error('[SiteList] ERROR creating chart:', error);
          console.error('[SiteList] Error stack:', error.stack);
          el.innerHTML = '<div style="color: red; padding: 20px;">Error creating chart: ' + error.message + '</div>';
        }
      },
      
      resize: function(width, height) {
        console.log('[SiteList] Resize called', { width: width, height: height });
        // Optional: handle widget resize if needed
      }
    };
  }
});

console.log('[SiteList] Widget binding registered');
