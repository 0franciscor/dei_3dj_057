import { ComponentFixture, TestBed } from '@angular/core/testing';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { DeliveryService } from 'src/app/Services/DeliveryService/delivery.service';
import { CreateDeliveryComponent } from './create-delivery.component';
import { CreateDeliveryComponentDialog } from './create-delivery.component';
import { MatDialogRef, MatDialogModule, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { MatCardModule } from '@angular/material/card';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';

describe('CreateDeliveryComponent', () => {
  let component: CreateDeliveryComponent;
  let fixture: ComponentFixture<CreateDeliveryComponent>;
  let dialogComponent: CreateDeliveryComponentDialog;
  let dialogFixture: ComponentFixture<CreateDeliveryComponentDialog>;
  let fakeDeliveryService: any;

  const dialogMock = {
    close: () => { }
  };

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [CreateDeliveryComponent, CreateDeliveryComponentDialog],
      imports: [MatDialogModule, FormsModule, ReactiveFormsModule, BrowserAnimationsModule, MatCardModule, MatFormFieldModule, MatInputModule],
      providers: [DeliveryService, { provide: MatDialogRef, useValue: dialogMock }, { provide: MAT_DIALOG_DATA, useValue: {} }]
    })
      .compileComponents();

    fakeDeliveryService = jasmine.createSpyObj('DeliveryService', ['createDelivery', 'createDeliveryProlog']);
    fakeDeliveryService.createDelivery.and.returnValue(Promise.resolve({ status: 201 }));

    TestBed.overrideProvider(DeliveryService, { useValue: fakeDeliveryService });
    fixture = TestBed.createComponent(CreateDeliveryComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
    component.ngOnInit();

    dialogFixture = TestBed.createComponent(CreateDeliveryComponentDialog);
    dialogComponent = dialogFixture.componentInstance;
    dialogFixture.detectChanges();
    dialogComponent.ngOnInit();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should create dialog', () => {
    expect(dialogComponent).toBeTruthy();
  });

  it('onSubmit with invalid form', async () => {
    await component.onSubmit();
    expect(component.formCreateDelivery.valid).toBeFalsy();
  });

  it('onSubmit with valid form', async () => {
    component.formCreateDelivery.controls['deliveryID'].setValue("123");
    component.formCreateDelivery.controls['deliveryDate'].setValue("2023-05-05");
    component.formCreateDelivery.controls['loadTime'].setValue(5);
    component.formCreateDelivery.controls['unloadTime'].setValue(5);
    component.formCreateDelivery.controls['destination'].setValue("1");
    component.formCreateDelivery.controls['deliveryMass'].setValue(5);
    await component.onSubmit();
    expect(component.formCreateDelivery.valid).toBeTruthy();
  });

  it('onSubmit with valid form, error on create', async () => {
    component.formCreateDelivery.controls['deliveryID'].setValue("123");
    component.formCreateDelivery.controls['deliveryDate'].setValue("2023-05-05");
    component.formCreateDelivery.controls['loadTime'].setValue(5);
    component.formCreateDelivery.controls['unloadTime'].setValue(5);
    component.formCreateDelivery.controls['destination'].setValue("1");
    component.formCreateDelivery.controls['deliveryMass'].setValue(1);
    fakeDeliveryService.createDelivery.and.returnValue(Promise.resolve({ status: 500 }));
    await component.onSubmit();
    expect(component.formCreateDelivery.valid).toBeTruthy();
  });

  it('onOk', async () => {
    const dialogSpy = spyOn(dialogComponent.dialogRef, 'close');
    dialogComponent.onOk();
    expect(dialogSpy).toHaveBeenCalled();
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

  it('should get a delivery', async () => {
    const response = {
      "deliveryID": "123",
      "deliveryDate": "2023-05-05",
      "loadTime": 5,
      "unloadTime": 5,
      "destination": "1",
      "deliveryMass": 5,
      json() {
        return this;
      }
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const delivery = await service.getDelivery("123");
    expect(fetchSpy).toHaveBeenCalled();
    expect(delivery).toEqual(response);
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

  it('should create a delivery', async () => {
    const response = {
      "status": 201
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const status = await service.createDelivery("123");
    expect(fetchSpy).toHaveBeenCalled();
    expect(status.status).toEqual(201);
  });

  it('should create a delivery with prolog', async () => {
    const response = {
      "status": 201
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const status = await service.createDeliveryProlog("123");
    expect(fetchSpy).toHaveBeenCalled();
    expect(status.status).toEqual(201);
  });

  it('should update a delivery', async () => {
    const response = {
      "status": 200
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const status = await service.updateDelivery("123");
    expect(fetchSpy).toHaveBeenCalled();
    expect(status.status).toEqual(200);
  });

  it('should update a delivery with prolog', async () => {
    const response = {
      "status": 200
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const status = await service.updateDeliveryProlog("123");
    expect(fetchSpy).toHaveBeenCalled();
    expect(status.status).toEqual(200);
  });

  it('should send a fetch without data', async () => {
    const status = await service.sendFetch('test', 'POST', 'null');
    expect(status.status).toEqual(404);
  }); 

  it('should send a fetch with data', async () => {
    const status = await service.sendFetch('test', 'POST', 'test');
    expect(status.status).toEqual(404);
  });
  
});