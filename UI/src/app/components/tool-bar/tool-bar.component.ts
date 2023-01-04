import { Component, OnInit, NgZone } from '@angular/core';

import { Router } from '@angular/router';
import { LoginService } from 'src/app/Services/LoginService/login.service';

@Component({
  selector: 'app-tool-bar',
  templateUrl: './tool-bar.component.html',
  styleUrls: ['./tool-bar.component.css'] 
})
export class ToolBarComponent implements OnInit {

  constructor(private ngZone:NgZone,private loginService:LoginService,private router: Router) {}

  

  isAdmn: boolean = false;
  isLoggedIn: boolean = false;
  async isAdmin() {
    const role = await this.loginService.getRole();
    if(role=="admin"){
      return true
    }
    else
      return false;
    
  }

  async ngOnInit() {
    this.isAdmn = await this.isAdmin();
    for (let cookie of document.cookie.split(';')) {
      const cookieName = cookie.split("=")[0].trim();
      if(cookieName == "jwt"){
        if(cookie.split("=")[1] == undefined){
          this.isLoggedIn = false;
          this.ngZone.run(() => this.router.navigate(['/login']));
        }
        else{
          this.isLoggedIn = true;
          break;
        }
      }
    }
    if(!this.isLoggedIn)
      this.ngZone.run(() => this.router.navigate(['/login']));
  }

  async goHome() {
    const role = await this.loginService.getRole();
    if(role == "admin"){
      this.ngZone.run(() => this.router.navigate(['/Admin/Home']));
    }
    else if(role == "whMan"){
      this.ngZone.run(() => this.router.navigate(['/WarehouseManagement/Home/WarehouseManager']));
    }
    else if(role == "logMan"){
      this.ngZone.run(() => this.router.navigate(['/Logistics/Home/LogisticsManager']));
    }
    else if(role == "fltMan"){
      this.ngZone.run(() => this.router.navigate(['/Logistics/Home/FleetManager']));
    }
    else
      this.ngZone.run(() => this.router.navigate(['/login']));


  }

  logout() {
    const cookies = document.cookie.split(';');

    for (let cookie of cookies) {
      const cookieName = cookie.split("=")[0].trim();
      document.cookie = `${cookieName}=; expires=Thu, 01 Jan 1970 00:00:00 UTC; path=/`
    }
    window.location.reload();
    this.ngZone.run(() => this.router.navigate(['/']));
  }


  goToWMHome(): void {
    this.ngZone.run(() => this.router.navigate(['/WarehouseManagement/Home/WarehouseManager']));
  }

  goToLMHome(): void {
    this.ngZone.run(() => this.router.navigate(['/Logistics/Home/LogisticsManager']));
  }

  goToFMHome(): void {
    this.ngZone.run(() => this.router.navigate(['/Logistics/Home/FleetManager']));
  }

  goToTermsAndConditions(): void {
    this.ngZone.run(() => this.router.navigate(['/TermsAndConditions']));
  }

  goToUserManagement(): void {
    this.ngZone.run(() => this.router.navigate(['/UserManagement/CancelAccount']));
  }
 

}
