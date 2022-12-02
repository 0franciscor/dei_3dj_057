import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { DeliveryService } from 'src/app/Services/DeliveryService/delivery.service';

@Component({
  selector: 'app-get-deliveries',
  templateUrl: './get-deliveries.component.html',
  styleUrls: ['./get-deliveries.component.css']
})
export class GetDeliveriesComponent implements OnInit {

  public selectedDeliveryOption: any;
  public selectedDelivery: any;

  public deliveryList: any[] = [];

  displayedColumns: string[] = ['deliveryID', 'deliveryDate', 'loadTime', 'unloadTime', 'destination', 'deliveryMass', 'Actions'];
  dataSource = this.deliveryList;

  constructor(private deliveryService: DeliveryService, private router: Router) {

    this.deliveryService.getDeliveries().then((data) => {
      this.deliveryList = data;
      this.dataSource = this.deliveryList;
    });
  }

  ngOnInit(): void {
  }

  goToEditDelivery(deliveryID : string) {
    this.router.navigate(['WarehouseManagement/Delivery/EditDelivery', deliveryID]);
  }
  
}