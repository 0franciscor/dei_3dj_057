import { Component, OnInit, NgZone } from '@angular/core';
import { Router } from '@angular/router';
import { LoginService } from 'src/app/Services/LoginService/login.service';

@Component({
  selector: 'app-admin-home',
  templateUrl: './admin-home.component.html',
  styleUrls: ['./admin-home.component.css']
})
export class AdminHomeComponent implements OnInit {

  constructor(private ngZone:NgZone,private loginService:LoginService, private router: Router) { }

  isAuth: boolean = false;
  authorizedRoles: string[] = ["admin"];
  async isAuthenticated() {
    const role= await this.loginService.getRole();
    if(!this.authorizedRoles.includes(role)){
      this.ngZone.run(() =>this.router.navigate(['/']))
      return false
    }
    else
      return true;
    
  }

  async ngOnInit() {
    this.isAuth = await this.isAuthenticated();
  }
  goToCreateUser(){
    this.ngZone.run(() => this.router.navigate(['Admin/CreateUser']));
  }

  goToCancelUser(){
    this.ngZone.run(() => this.router.navigate(['Admin/CancelUser']));
  }



}
