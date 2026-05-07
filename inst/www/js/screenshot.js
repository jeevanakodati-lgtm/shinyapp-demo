$(document).on("click", ".screenshot-btn", function () {
  const targetId = $(this).data("target");
  const filename = $(this).data("filename");
  const element = document.getElementById(targetId);

  if (!element) {
    alert("Plot not found.");
    return;
  }

  html2canvas(element, {
    backgroundColor: "#ffffff",
    scale: 2
  }).then(function (canvas) {
    const link = document.createElement("a");
    link.download = filename;
    link.href = canvas.toDataURL("image/png");
    link.click();
  });
});
