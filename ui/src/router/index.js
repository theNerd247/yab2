import Vue from 'vue'
import Router from 'vue-router'
import Yab from '@/components/Yab.vue'

Vue.use(Router)

export default new Router({
  routes: [
    {
      path: '/',
      name: 'Yab',
      component: Yab
    }
  ]
})
