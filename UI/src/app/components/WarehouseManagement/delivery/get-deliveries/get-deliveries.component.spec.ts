import { ComponentFixture, TestBed } from '@angular/core/testing';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MatCardModule } from '@angular/material/card';
import { MatDialogModule, MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';

import { DeliveryService } from 'src/app/Services/DeliveryService/delivery.service';
import { GetDeliveriesComponent } from './get-deliveries.component';

describe('GetDeliveriesComponent', () => {
  let component: GetDeliveriesComponent;
  let fixture: ComponentFixture<GetDeliveriesComponent>;
  let fakeDeliveryService: any;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [GetDeliveriesComponent],
      imports: [MatDialogModule, FormsModule, ReactiveFormsModule, BrowserAnimationsModule, MatCardModule, MatFormFieldModule, MatInputModule],
      providers: [DeliveryService, { provide: MatDialogRef, useValue: {} }, { provide: MAT_DIALOG_DATA, useValue: {} }]

    })
      .compileComponents();

    fakeDeliveryService = jasmine.createSpyObj('DeliveryService', ['getDeliveries']);
    fakeDeliveryService.getDeliveries.and.returnValue(Promise.resolve({ status: 200 }));

    TestBed.overrideProvider(DeliveryService, { useValue: fakeDeliveryService });
    fixture = TestBed.createComponent(GetDeliveriesComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
    component.ngOnInit();

  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});

describe('DeliveryService', () => {
  let service: DeliveryService;

  beforeEach(() => {
    TestBed.configureTestingModule({});
    service = TestBed.inject(DeliveryService);
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });

  it('should get all deliveries', async () => {
    const response = {
      "deliveries": [
        {
          "deliveryID": "123",
          "deliveryDate": "2023-05-05",
          "loadTime": 5,
          "unloadTime": 5,
          "destination": "1",
          "deliveryMass": 5
        }
      ],
      json() {
        return this;
      }
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const deliveries = await service.getDeliveries();
    expect(fetchSpy).toHaveBeenCalled();
    expect(deliveries).toEqual(response);
  });

});