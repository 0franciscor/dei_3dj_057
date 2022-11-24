import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { WarehouseService } from 'src/app/Services/WarehouseService/warehouse.service';

interface warehouse{
  warehouseId: string;
  address: string;
  altitude: number;
  latitude: string;
  longitude: string;
  description: string;
  cityId: string;
}

@Component({
  selector: 'app-warehouse-by-id',
  templateUrl: './warehouse-by-id.component.html',
  styleUrls: ['./warehouse-by-id.component.css']
})

export class WarehouseByIdComponent implements OnInit {

  public selectedWarehouseOption : any;
  public selectedWarehouse: any;
  
  public warehouseList: any[] = [];
  
  constructor(private warehouseService: WarehouseService, private router: Router) { 
    
    this.warehouseService.getAllWarehouses().then((data) => {
      this.warehouseList = data;
      console.log(this.warehouseList);
    });
    

    this.selectedWarehouse= {
      warehouseId: "",
      address: undefined,
      altitude: undefined,
      latitude: undefined,
      longitude: undefined,
      description: undefined,
      cityId: undefined
    }
  }


  ngOnInit(): void {
    
  }

  onWarehouseSelected($event: any){

    let test = this.warehouseList.find(element => element.id == this.selectedWarehouseOption);

    console.log(test)
    this.selectedWarehouse = test;
    
  }

}
