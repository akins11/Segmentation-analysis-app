$(document).on('click', '.monitor', function () {
 Shiny.setInputValue('assign_segments-last_click', this.id);
});
