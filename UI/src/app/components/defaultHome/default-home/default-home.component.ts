import { Component, OnInit, NgZone } from '@angular/core';
import { Router } from '@angular/router';
import { LoginService } from 'src/app/Services/LoginService/login.service';

@Component({
  selector: 'app-default-home',
  templateUrl: './default-home.component.html',
  styleUrls: ['./default-home.component.css']
})
export class DefaultHomeComponent implements OnInit {

  constructor(private ngZone:NgZone,private router: Router, private loginService: LoginService) { }

  async ngOnInit() {

    const role = await this.loginService.getRole();
    if(role=="admin")
      this.ngZone.run(() => this.router.navigate(['/Admin/Home']));
    else if(role=="whMan" )
      this.ngZone.run(() => this.router.navigate(['/WarehouseManagement/Home/WarehouseManager']));  
    else if(role=="logMan")
      this.ngZone.run(() => this.router.navigate(['/Logistics/Home/LogisticsManager']));
    else if(role=="fltMan")
      this.ngZone.run(() => this.router.navigate(['/Logistics/Home/FleetManager']));
    else
      this.ngZone.run(() => this.router.navigate(['/login']));
    
  }

}
