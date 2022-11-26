import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { ActivatedRoute, Router } from '@angular/router';
import { DeliveryService } from 'src/app/Services/DeliveryService/delivery.service';

@Component({
  selector: 'app-edit-delivery',
  templateUrl: './edit-delivery.component.html',
  styleUrls: ['./edit-delivery.component.css']
})
export class EditDeliveryComponent implements OnInit {

  formEditDelivery!: FormGroup;
  minDate: Date;

  constructor(private route: ActivatedRoute, private deliveryService: DeliveryService, private fb: FormBuilder, private router: Router) { 
    const currentYear = new Date().getFullYear();
    const currentMonth = new Date().getMonth();
    const currentDay = new Date().getDate();
    this.minDate = new Date(currentYear, currentMonth, currentDay);
  }

  selectedDelivery = {
    deliveryID: "",
    deliveryDate: undefined,
    loadTime: undefined,
    unloadTime: undefined,
    destination: undefined,
    deliveryMass: undefined,
  }

  ngOnInit(): void {
    const deliveryID = this.route.snapshot.paramMap.get('id');
    this.formEditDelivery = this.fb.group({
      deliveryID: this.selectedDelivery.deliveryID,
      deliveryDate: this.selectedDelivery.deliveryDate,
      loadTime: this.selectedDelivery.loadTime,
      unloadTime: this.selectedDelivery.unloadTime,
      destination: this.selectedDelivery.destination,
      deliveryMass: this.selectedDelivery.deliveryMass
    });

    if (deliveryID)
      this.deliveryService.getDelivery(deliveryID).then((data) => {
        this.selectedDelivery = data;
        this.formEditDelivery = this.fb.group({
          deliveryID: this.selectedDelivery.deliveryID,
          deliveryDate: this.selectedDelivery.deliveryDate,
          loadTime: this.selectedDelivery.loadTime,
          unloadTime: this.selectedDelivery.unloadTime,
          destination: this.selectedDelivery.destination,
          deliveryMass: this.selectedDelivery.deliveryMass
        });
      });
  }

  async onSubmit() {
    let answer = await this.deliveryService.updateDelivery(this.formEditDelivery.value);
    let message!: string;
    
    if (answer.status != 200)
      message = "Error updating Delivery";

    if (answer.status == 200)
      this.router.navigate(['WarehouseManagement/Delivery/GetDelivery']);

  }

}
