import { ComponentFixture, TestBed } from '@angular/core/testing';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MatCardModule } from '@angular/material/card';
import { MatDialogModule, MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { WarehouseService } from 'src/app/Services/WarehouseService/warehouse.service';

import { CreateWarehouseComponent, CreateWarehouseComponentDialog } from './create-warehouse.component';

describe('CreateWarehouseComponent', () => {
  let component: CreateWarehouseComponent;
  let fixture: ComponentFixture<CreateWarehouseComponent>;
  let dialogComponent: CreateWarehouseComponentDialog;
  let dialogFixture: ComponentFixture<CreateWarehouseComponentDialog>;
  let fakeWarehouseService: any;

  const dialogMock = {
    close: () => { }
  };

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ CreateWarehouseComponent,CreateWarehouseComponentDialog ],
      imports: [MatDialogModule,FormsModule,ReactiveFormsModule,
        BrowserAnimationsModule,MatCardModule,MatFormFieldModule,MatInputModule],
        providers: [WarehouseService, {provide: MatDialogRef, useValue: dialogMock},{ provide: MAT_DIALOG_DATA, useValue: {} },]
    })
    .compileComponents();

    fakeWarehouseService = jasmine.createSpyObj('WarehouseService', ['createWarehouse','createWarehouseProlog']);
    fakeWarehouseService.createWarehouse.and.returnValue(Promise.resolve({status: 200}));

    TestBed.overrideProvider(WarehouseService, {useValue: fakeWarehouseService});
    fixture = TestBed.createComponent(CreateWarehouseComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
    component.ngOnInit();

    dialogFixture = TestBed.createComponent(CreateWarehouseComponentDialog);
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
    expect(component.formCreateWarehouse.valid).toBeFalsy();
  });


  it('onSubmit with valid form', async () => {
    component.formCreateWarehouse.controls['Id'].setValue('TH1');
    component.formCreateWarehouse.controls['Address'].setValue('Rua António Bernardino,47,4535-334,Porto');
    component.formCreateWarehouse.controls['Altitude'].setValue(200);
    component.formCreateWarehouse.controls['Latitude'].setValue('40.9321º N');
    component.formCreateWarehouse.controls['Longitude'].setValue('8.2451º W');
    component.formCreateWarehouse.controls['Designation'].setValue('Porto');
    await component.onSubmit();
    expect(component.formCreateWarehouse.valid).toBeTruthy();
  });


  it('onSubmit with valid form, error on create', async () => {
    component.formCreateWarehouse.controls['Id'].setValue('TH1');
    component.formCreateWarehouse.controls['Address'].setValue('Rua António Bernardino,47,4535-334,Porto');
    component.formCreateWarehouse.controls['Altitude'].setValue(200);
    component.formCreateWarehouse.controls['Latitude'].setValue('40.9321º N');
    component.formCreateWarehouse.controls['Longitude'].setValue('8.2451º W');
    component.formCreateWarehouse.controls['Designation'].setValue('Porto');
    fakeWarehouseService.createWarehouse.and.returnValue(Promise.resolve({status: 500}));
    await component.onSubmit();
    expect(component.formCreateWarehouse.valid).toBeTruthy();
  });

  it('onOk', async () => {
    
    const dialogSpy = spyOn(dialogComponent.dialogRef, 'close');
    
    dialogComponent.onOk();
    expect(dialogSpy).toHaveBeenCalled();
  });


});

describe('WarehouseService', () => {
  let service: WarehouseService;

  beforeEach(() => {
    TestBed.configureTestingModule({});
    service = TestBed.inject(WarehouseService);
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });

  it('should get a warehouse', async () => {
    const response = {
        "id": "TH1",
        "address": "Rua António Bernardino,47,4535-334,Porto",
        "altitude": 250,
        "latitude": "40.9321º N",
        "longitude": "8.2451º W",
        "designation": "Arouca",
        "city": "1",
      json () {
        return this;
      }
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const warehouse = await service.getWarehouse('TH1');
    expect(fetchSpy).toHaveBeenCalled();
    expect(warehouse).toEqual(response);
  });


  it('should create a warehouse', async () => {
    const response = {
      "status": 200,
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const status = await service.createWarehouse('TH1');
    expect(fetchSpy).toHaveBeenCalled();
    expect(status.status).toEqual(200);
  });

  it('should create a warehouse prolog', async () => {
    const response = {
      "status": 201,
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const status = await service.createWarehouseProlog('TH1');
    expect(fetchSpy).toHaveBeenCalled();
    expect(status.status).toEqual(201);

  });

  it('should update a warehouse', async () => {
    const response = {
      "status": 200,
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const status = await service.updateWarehouse('TH1');
    expect(fetchSpy).toHaveBeenCalled();
    expect(status.status).toEqual(200);
  });

  it('should update a warehouse prolog', async () => {
    const response = {
      "status": 200,
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const status = await service.updateWarehouseProlog('TH1');
    expect(fetchSpy).toHaveBeenCalled();
    expect(status.status).toEqual(200);
  });


  it('should get all warehouses', async () => {
    const response = {
      "warehouses": [
        {
          "id": "TH1",
          "address": "Rua António Bernardino,47,4535-334,Porto",
          "altitude": 250,
          "latitude": "40.9321º N",
          "longitude": "8.2451º W",
          "designation": "Arouca",
          "city": "1",
        }
      ],
      json () {
        return this;
      }
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const trucks = await service.getAllWarehouses();
    expect(fetchSpy).toHaveBeenCalled();
    expect(trucks).toEqual(response);
  });

  it('should send a fetch without data', async () => {

    const status = await service.sendFetch('test', 'GET', null);
    expect(status.status).toEqual(404);
  });

  it('should send a fetch with data', async () => {
    const status = await service.sendFetch('test', 'POST', "null");
    expect(status.status).toEqual(404);
  });

});
