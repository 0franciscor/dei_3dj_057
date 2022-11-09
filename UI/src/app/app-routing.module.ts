import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { LogInComponent } from './components/log-in/log-in.component';
import { CubeComponent } from './cube/cube.component';
// import { RegisterComponent } from './components/register/register.component';
import { CreateTruckComponent } from './components/Logistics/truck/create-truck/create-truck.component';


const routes: Routes = [
  { path:'cube', component: CubeComponent },
  { path: 'login', component: LogInComponent },
  { path: 'Logistics/Truck/CreateTruck', component: CreateTruckComponent },
];

@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule]
})
export class AppRoutingModule { }
