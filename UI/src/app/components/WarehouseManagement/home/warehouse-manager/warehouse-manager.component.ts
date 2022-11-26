import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import {WarehouseService} from "../../../../Services/WarehouseService/warehouse.service";
import { DeliveryService } from "src/app/Services/DeliveryService/delivery.service";

@Component({
  selector: 'app-warehouse-manager',
  templateUrl: './warehouse-manager.component.html',
  styleUrls: ['./warehouse-manager.component.css']
})
export class WarehouseManagerComponent implements OnInit {

  constructor(private warehouseService: WarehouseService, private deliveryService : DeliveryService, private router: Router) {

  }

  ngOnInit(): void {
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

}