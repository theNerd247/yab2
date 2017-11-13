import Vue from 'vue'
import Router from 'vue-router'
import Yab from '@/components/Yab.vue'
import Home from '@/components/pages/Home.vue'
import Budget from '@/components/pages/Budget.vue'
import BudgetMenu from '@/components/pages/BudgetMenu.vue'
import Expenses from '@/components/pages/Expenses.vue'

Vue.use(Router)

export default new Router({
  routes: [
    {
      path: '/',
      name: 'Yab',
      component: Yab,
      children: [
        { path: '',
          name: 'Home',
          component: Home
        },
				{ path: 'budget/',
          name: 'BudgetMenu',
          component: BudgetMenu
        },
				{ path: 'budget/:name',
          name: 'Budget',
          component: Budget
        },
				{ path: 'expenses/:name',
          name: 'Expenses',
          component: Expenses
        }
      ]
    }
  ]
})
