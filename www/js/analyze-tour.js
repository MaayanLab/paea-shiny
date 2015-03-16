// See: https://gist.github.com/xiaodaigh/6445698
var swichTab = function(tabId, i) {
    tabs = $(tabId).find('li');
    tabs.each(function() {
        $(this).removeClass('active')
    });
    $(tabs[i]).addClass('active');
    tabsContents = $(tabId).next('.tab-content').children('.tab-pane');
    tabsContents.each(function() {
        $(this).removeClass('active')
    });
    $(tabsContents[i]).addClass('active');
}

function initTour() {
    
    var steps = [{
            content: '<p>Welcome...</p>',
            target: [50, 120],
            nextButton: true,
            my: 'bottom left'
    }, {
        content: [
            '<p>Choose expression dataset to upload.</p>',
            '<p>It should contain a sinlge column with gene names ',
            'followed by the  expression profiles. </p>',
            '<p>If you want to test this app you can use ',
            '<a href="https://www.dropbox.com/s/151k3l70toseymi/expression_example.csv">',
            'expression_example.csv</a>.</p>'
        ].join(''),
        highlightTarget: true,
        nextButton: true,
        closeButton: true,
        target: $('#datain_container'),
        my: 'top left',
        at: 'bottom center',
    }, {
        content: [
            '<p>Select input file separator</p>'
        ].join(''),
        highlightTarget: true,
        nextButton: true,
        closeButton: true,
        target: $('#sep'),
        my: 'bottom center',
        at: 'top center'
    }, {
        content: [
            '<p>Check if file has been loaded as expected.</p>'
        ].join(''),
        highlightTarget: true,
        nextButton: true,
        closeButton: true,
        target: $('#datain_preview'),
        my: 'bottom center',
        at: 'top center'
    }, {
        content: [
            '<p>Choose control samples</p>'
        ].join(''),
        highlightTarget: true,
        nextButton: true,
        closeButton: true,
        target: $('#control_samples'),
        my: 'bottom center',
        at: 'top center'
    }, {
        content: [
            '<p>If you prefer you can use disease signature dataset ',
            'obtained from the Gene Expression Omnibus</p>'
        ].join(''),
        highlightTarget: true,
        nextButton: true,
        closeButton: true,
        target: $('#datain_type'),
        my: 'top left',
        at: 'bottom right',
        teardown: function(tour, options) {
            $('input:radio[name=datain_type]').filter('[value=disease]').trigger('click');
        }
    }, {
        content: [
            '<p>Add preprocessing steps</p>'
        ].join(''),
        highlightTarget: true,
        nextButton: true,
        closeButton: true,
        target: $('#datain_preprocessing'),
        my: 'bottom center',
        at: 'top center'
    }, {
        content: [
            '<p>Switch to the Characteristic Direction Analysis</p>'
        ].join(''),
        highlightTarget: true,
        nextButton: true,
        closeButton: true,
        target: $('#workflow_panel'),
        my: 'top center',
        at: 'bottom center',
        teardown: function(tour, options) {
            swichTab('#workflow_panel', 1);
        }
    }, {
        content: [
            '<p>Check input data summary</p>'
        ].join(''),
        nextButton: true,
        closeButton: true,
        target: $('#chdir_input_summary'),
        my: 'bottom center',
        at: 'bottom center'
    }, {
        content: [
            '<p>Set parameters...</p>'
        ].join(''),
        nextButton: true,
        closeButton: true,
        target: $('#chdir_parameters'),
        my: 'bottom center',
        at: 'bottom center'
    }, {
        content: [
            '<p>...and run chdir</p>'
        ].join(''),
        highlightTarget: true,
        nextButton: true,
        closeButton: true,
        target: $('#run_chdir_container'),
        my: 'bottom center',
        at: 'top center'
    }, {
        content: [
            '<p>When chdir analysis is finished ',
            'you can download the results...</p>'
        ].join(''),
        highlightTarget: true,
        nextButton: true,
        closeButton: true,
        target: $('#chdir_downloads'),
        my: 'bottom center',
        at: 'bottom center'
    }, {
        content: [
            '<p>...send results to the Enrichr...</p>'
        ].join(''),
        highlightTarget: true,
        nextButton: true,
        closeButton: true,
        target: $('#enrichr_form'),
        my: 'top center',
        at: 'bottom center'
    }, {
        content: [
            '<p>or start PAE analysis</p>'
        ].join(''),
        highlightTarget: true,
        nextButton: true,
        closeButton: true,
        target: $('#workflow_panel'),
        my: 'bottom center',
        at: 'bottom center',
        teardown: function(tour, options) {
            swichTab('#workflow_panel', 2);
        }
    }, {
        content: [
            '<p>To run PAEA click Run Principle Angle Enrichment</p>'
        ].join(''),
        highlightTarget: true,
        nextButton: true,
        closeButton: true,
        target: $('#run_paea_container'),
        my: 'bottom center',
        at: 'top center'
    }, {
        content: [
            '<p>When PAEA is ready results will be shown below</p>'
        ].join(''),
        highlightTarget: true,
        nextButton: true,
        closeButton: true,
        target: $('#pae_results'),
        my: 'top left',
        at: 'top left'
    },  {
        content: [
            '<p>... and will be available for download</p>'
        ].join(''),
        highlightTarget: true,
        nextButton: true,
        closeButton: true,
        target: $('#paea_downloads'),
        my: 'top left',
        at: 'top left'
    }];

    var tour = new Tourist.Tour({
        steps: steps,
        tipClass: 'Bootstrap',
        tipOptions:{ showEffect: 'slidein' }
    });
    tour.start();
}

$(document).ready(function() {
    $('#start_tour').click(initTour);
})