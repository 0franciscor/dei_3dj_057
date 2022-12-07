import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';
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

  displayedColumns: string[] = ['warehouseID', 'address', 'altitude', 'latitude', 'longitude', 'description','cityId', 'active' ,'edit', 'activate'];
  dataSource = this.warehouseList;

  
  constructor(private warehouseService: WarehouseService, private router: Router) { 
    
    this.warehouseService.getAllWarehouses().then((data) => {
      this.warehouseList = data;
      this.dataSource = this.warehouseList;
    });
  }
  
  ngOnInit(): void {
  }

  goToEditWarehouse(warehouseID : string) {
    this.router.navigate(['WarehouseManagement/Warehouse/EditWarehouse', warehouseID]);
  }

  goToActiveWarehouse(warehouseID : string) {
    this.warehouseService.activateWarehouse(warehouseID)
  }

}
