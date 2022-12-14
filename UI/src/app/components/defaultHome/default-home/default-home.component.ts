import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { LoginService } from 'src/app/Services/LoginService/login.service';

@Component({
  selector: 'app-default-home',
  templateUrl: './default-home.component.html',
  styleUrls: ['./default-home.component.css']
})
export class DefaultHomeComponent implements OnInit {

  constructor(private router: Router, private loginService: LoginService) { }

  async ngOnInit() {
    //TODO: check if admin, if so, redirect to admin page
    const role = await this.loginService.getRole();
    if(role=="whMan" )
      this.router.navigate(['/WarehouseManagement/Home/WarehouseManager']);
    else if(role=="logMan")
      this.router.navigate(['/Logistics/Home/LogisticsManager']);
    else if(role=="fltMan")
      this.router.navigate(['/Logistics/Home/FleetManager']);
    else
      this.router.navigate(['/login']);
    
  }

}
