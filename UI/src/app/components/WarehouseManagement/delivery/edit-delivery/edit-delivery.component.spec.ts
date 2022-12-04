import { ComponentFixture, TestBed } from '@angular/core/testing';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { RouterTestingModule } from '@angular/router/testing';
import { DeliveryService } from 'src/app/Services/DeliveryService/delivery.service';
import { EditDeliveryComponent } from './edit-delivery.component';
import { EditDeliveryComponentDialog } from './edit-delivery.component';
import { MatDialogRef, MatDialogModule, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { MatCardModule } from '@angular/material/card';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { ActivatedRoute } from '@angular/router';

describe('EditDeliveryComponent', () => {
  let component: EditDeliveryComponent;
  let fixture: ComponentFixture<EditDeliveryComponent>;
  let dialogComponent: EditDeliveryComponentDialog;
  let dialogFixture: ComponentFixture<EditDeliveryComponentDialog>;
  let fakeDeliveryService: any;

  const dialogMock = {
    close: () => { }
  };

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [EditDeliveryComponent, EditDeliveryComponentDialog],
      imports: [MatDialogModule, FormsModule, ReactiveFormsModule, BrowserAnimationsModule, MatCardModule, MatFormFieldModule, MatInputModule, RouterTestingModule.withRoutes(
        [{ path: 'WarehouseManagement/Home/WarehouseManager', redirectTo: '' }],
      )],
      providers: [DeliveryService, { provide: MatDialogRef, useValue: dialogMock }, { provide: MAT_DIALOG_DATA, useValue: {} }, {
        provide: ActivatedRoute,
        useValue: {
          snapshot: {
            paramMap: {
              get: () => '1'
            }
          }
        }
      }]
    })
      .compileComponents();

    fakeDeliveryService = jasmine.createSpyObj('DeliveryService', ['updateDelivery', 'getDelivery', 'updateDeliveryProlog']);
    fakeDeliveryService.updateDelivery.and.returnValue(Promise.resolve({ status: 200 }));

    fakeDeliveryService.getDelivery.and.returnValue(Promise.resolve({
      deliveryID: "123",
      deliveryDate: "2023-05-05",
      loadTime: 5,
      unloadTime: 5,
      destination: "1",
      deliveryMass: 5
    }));

    TestBed.overrideProvider(DeliveryService, { useValue: fakeDeliveryService });
    fixture = TestBed.createComponent(EditDeliveryComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
    component.ngOnInit();

    dialogFixture = TestBed.createComponent(EditDeliveryComponentDialog);
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

  it('onOk', async () => {
    const dialogSpy = spyOn(dialogComponent.dialogRef, 'close');

    dialogComponent.onOk();
    expect(dialogSpy).toHaveBeenCalled();
  });

  it('component finishes on submit', async () => {
    await component.onSubmit();
  });

  it('onGoBack', async () => {
    component.goBack();
  });

  it('should call onSubmit', async () => {
    spyOn(component, 'onSubmit');
    await component.onSubmit();
    expect(component.onSubmit).toHaveBeenCalled();
  });

  it('should call onSubmit with DeliveryID', async () => {
    spyOn(component, 'onSubmit');
    component.formEditDelivery.controls['deliveryID'].setValue("123");
    await component.onSubmit();
    expect(component.onSubmit).toHaveBeenCalled();
  });

  it('should call onSubmit with error', async () => {
    fakeDeliveryService.updateDelivery.and.returnValue(Promise.resolve({ status: 500 }));
    await component.onSubmit();
  });
});

describe('EditDeliveryComponent with null activated route', () => {
  let component: EditDeliveryComponent;
  let fixture: ComponentFixture<EditDeliveryComponent>;
  let dialogComponent: EditDeliveryComponentDialog;
  let dialogFixture: ComponentFixture<EditDeliveryComponentDialog>;
  let fakeDeliveryService: any;

  const dialogMock = {
    close: () => { }
  };

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [EditDeliveryComponent, EditDeliveryComponentDialog],
      imports: [MatDialogModule, FormsModule, ReactiveFormsModule, BrowserAnimationsModule, MatCardModule, MatFormFieldModule, MatInputModule, RouterTestingModule.withRoutes(
        [{ path: 'WarehouseManagement/Home/WarehouseManager', redirectTo: '' }],
      )],
      providers: [DeliveryService, { provide: MatDialogRef, useValue: dialogMock }, { provide: MAT_DIALOG_DATA, useValue: {} }, {
        provide: ActivatedRoute,
        useValue: {
          snapshot: {
            paramMap: {
              get: () => null
            }
          }
        }
      }]
    })
      .compileComponents();

    fakeDeliveryService = jasmine.createSpyObj('DeliveryService', ['updateDelivery', 'getDelivery', 'updateDeliveryProlog']);
    fakeDeliveryService.updateDelivery.and.returnValue(Promise.resolve({ status: 200 }));

    fakeDeliveryService.getDelivery.and.returnValue(Promise.resolve({
      deliveryID: "123",
      deliveryDate: "2023-05-05",
      loadTime: 5,
      unloadTime: 5,
      destination: "1",
      deliveryMass: 5
    }));

    TestBed.overrideProvider(DeliveryService, { useValue: fakeDeliveryService });
    fixture = TestBed.createComponent(EditDeliveryComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
    component.ngOnInit();

    dialogFixture = TestBed.createComponent(EditDeliveryComponentDialog);
    dialogComponent = dialogFixture.componentInstance;
    dialogFixture.detectChanges();
    dialogComponent.ngOnInit();

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

  it('should call getDelivery', async () => {
    const response = {
      deliveryID: "123",
      deliveryDate: "2023-05-05",
      loadTime: 5,
      unloadTime: 5,
      destination: "1",
      deliveryMass: 5,
      json() {
        return this;
      }
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const delivery = await service.getDelivery("123");
    expect(fetchSpy).toHaveBeenCalled();
    expect(delivery).toEqual(response);
  });


  it('should create a delivery', async () => {
    const response = {
      "status": 200,
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const status = await service.updateDelivery('123');
    expect(fetchSpy).toHaveBeenCalled();
    expect(status.status).toEqual(200);
  });

  it('should create a delivery prolog', async () => {
    const response = {
      "status": 201,
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const status = await service.createDeliveryProlog('123');
    expect(fetchSpy).toHaveBeenCalled();
    expect(status.status).toEqual(201);
  });

  it('should update a delivery', async () => {
    const response = {
      "status": 200,
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const status = await service.updateDelivery('123');
    expect(fetchSpy).toHaveBeenCalled();
    expect(status.status).toEqual(200);
  });

  it('should update a delivery prolog', async () => {
    const response = {
      "status": 200,
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const status = await service.updateDeliveryProlog('123');
    expect(fetchSpy).toHaveBeenCalled();
    expect(status.status).toEqual(200);
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

  it('should send a fetch without data', async () => {
    const status = await service.sendFetch('test', 'GET', null);
    expect(status.status).toEqual(404);
  });

  it('should send a fetch with data', async () => {
    const status = await service.sendFetch('test', 'POST', 'null');
    expect(status.status).toEqual(404);
  });
});

