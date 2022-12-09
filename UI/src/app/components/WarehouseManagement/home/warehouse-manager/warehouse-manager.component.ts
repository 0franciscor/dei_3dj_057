import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import {WarehouseService} from "../../../../Services/WarehouseService/warehouse.service";
import { DeliveryService } from "src/app/Services/DeliveryService/delivery.service";
import { LoginService } from 'src/app/Services/LoginService/login.service';

@Component({
  selector: 'app-warehouse-manager',
  templateUrl: './warehouse-manager.component.html',
  styleUrls: ['./warehouse-manager.component.css']
})
export class WarehouseManagerComponent implements OnInit {

  constructor(private loginService:LoginService,private warehouseService: WarehouseService, private deliveryService : DeliveryService, private router: Router) {

  }

  isAuth: boolean = false;
  authorizedRoles: string[] = ["whMan","admin"];
  async isAuthenticated() {
    const role= await this.loginService.getRole();
    if(!this.authorizedRoles.includes(role)){
      this.router.navigate(['/']);
      return false
    }
    else
      return true;
    
  }

  async ngOnInit() {
    this.isAuth = await this.isAuthenticated();
  }

  goToCreateWarehouse(){
    this.router.navigate(['WarehouseManagement/Warehouse/CreateWarehouse'])
  }

  goToCreateDelivery(){
    this.router.navigate(['WarehouseManagement/Delivery/CreateDelivery'])
  }

  goToGetWarehouseById(){
    this.router.navigate(['WarehouseManagement/Warehouse/GetWarehouseById'])
  }

  goToGetDelivery(){
    this.router.navigate(['WarehouseManagement/Delivery/GetDelivery'])
  }

  goToEditWarehouse(){
    this.router.navigate(['WarehouseManagement/Warehouse/EditWarehouse'])
  }

}