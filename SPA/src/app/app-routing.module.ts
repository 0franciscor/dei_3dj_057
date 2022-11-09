import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { LogInComponent } from './components/log-in/log-in.component';
import { CubeComponent } from './cube/cube.component';
// import { RegisterComponent } from './components/register/register.component';

const routes: Routes = [
  { path:'cube', component: CubeComponent },
  { path: 'login', component: LogInComponent }
];

@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule]
})
export class AppRoutingModule { }
