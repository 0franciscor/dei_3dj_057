import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { DeliveryService } from 'src/app/Services/DeliveryService/delivery.service';
import { LoginService } from 'src/app/Services/LoginService/login.service';

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

  constructor(private loginService:LoginService,private deliveryService: DeliveryService, private router: Router) {

    this.deliveryService.getDeliveries().then((data) => {
      this.deliveryList = data;
      this.deliveryList.forEach((delivery) => {
        if(delivery.deliveryID == "" || delivery.deliveryID == null) {
          delivery.deliveryID = "N/A";
        }
      });
      this.dataSource = this.deliveryList;
    });
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

  goToEditDelivery(deliveryID : string) {
    this.router.navigate(['WarehouseManagement/Delivery/EditDelivery', deliveryID]);
  }
}