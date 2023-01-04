import { Component, OnInit, NgZone } from '@angular/core';
import { Router } from '@angular/router';
import { LoginService } from 'src/app/Services/LoginService/login.service';
import { WarehouseService } from 'src/app/Services/WarehouseService/warehouse.service';

@Component({
  selector: 'app-get-warehouses',
  templateUrl: './get-warehouses.component.html',
  styleUrls: ['./get-warehouses.component.css']
})


export class GetWarehousesComponent implements OnInit {

  public selectedWarehouseOption : any;
  public selectedWarehouse: any;
  
  public warehouseList: any[] = [];

  displayedColumns: string[] = ['warehouseID', 'address', 'altitude', 'latitude', 'longitude', 'description','cityId', 'active' ,'edit'];
  dataSource = this.warehouseList;

  
  constructor(private ngZone:NgZone,private loginService:LoginService,private warehouseService: WarehouseService, private router: Router){ 
    
    this.warehouseService.getAllWarehouses().then((data) => {
      this.warehouseList = data;
      this.dataSource = this.warehouseList;
    });
  }
  
  isAuth: boolean = false;
  authorizedRoles: string[] = ["whMan","admin"];
  async isAuthenticated() {
    const role= await this.loginService.getRole();
    if(!this.authorizedRoles.includes(role)){
      this.ngZone.run(() => this.router.navigate(['/']));
      return false
    }
    else
      return true;
    
  }

  async ngOnInit() {
    this.isAuth = await this.isAuthenticated();
  }

  goToEditWarehouse(warehouseID : string) {
    this.ngZone.run(() => this.router.navigate(['WarehouseManagement/Warehouse/EditWarehouse', warehouseID]));
  }

  goToActiveWarehouse(warehouseID : string) {
    this.warehouseService.activateWarehouse(warehouseID)
    this.ngZone.run(() => this.router.navigate(['WarehouseManagement/Home/WarehouseManager']));

  }

  goToDeactiveWarehouse(warehouseID : string) {
    this.warehouseService.deactivateWarehouse(warehouseID)
    this.ngZone.run(() => this.router.navigate(['WarehouseManagement/Home/WarehouseManager']));

  }


}
