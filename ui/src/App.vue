<template>
	<div id="app"> <router-view/> </div>
</template>

<script>
	export default {
		name: 'app'
	}
</script>

<style>
	#app {
		font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
		-webkit-font-smoothing: antialiased;
		-moz-osx-font-smoothing: grayscale;
	}

	h1 {
		display: block;
		font-size: 2em;
		-webkit-margin-before: 0.67em;
		-webkit-margin-after: 0.67em;
		-webkit-margin-start: 0px;
		-webkit-margin-end: 0px;
		font-weight: bold;
	}
</style>
