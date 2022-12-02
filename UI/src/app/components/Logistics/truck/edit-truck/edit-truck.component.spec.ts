import { ComponentFixture, TestBed } from '@angular/core/testing';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MatCardModule } from '@angular/material/card';
import { MatDialogModule, MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { ActivatedRoute, RouterModule } from '@angular/router';
import { RouterTestingModule } from '@angular/router/testing';
import { TruckService } from 'src/app/Services/TruckService/truck.service';

import { EditTruckComponent, EditTruckComponentDialog } from './edit-truck.component';






describe('EditTruckComponent', () => {
  let component: EditTruckComponent;
  let fixture: ComponentFixture<EditTruckComponent>;
  let dialogComponent: EditTruckComponentDialog;
  let dialogFixture: ComponentFixture<EditTruckComponentDialog>;
  let fakeTruckService: any;
  const dialogMock = {
    close: () => { }
  };

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ EditTruckComponent, EditTruckComponentDialog ],
      imports: [MatDialogModule,FormsModule,ReactiveFormsModule, BrowserAnimationsModule,MatCardModule,MatFormFieldModule,MatInputModule, RouterTestingModule.withRoutes(
        [{path: 'Logistics/Home/FleetManager', redirectTo: ''}]
      )],
      providers: [TruckService, {provide: MatDialogRef, useValue: dialogMock},{ provide: MAT_DIALOG_DATA, useValue: {} },{provide: ActivatedRoute,
        useValue: {
          snapshot: {
            paramMap: {
              get: () => '1'
            }
          }
        }}]
    })
    .compileComponents();

    fakeTruckService = jasmine.createSpyObj('TruckService', ['updateTruck', 'getTruck','updateTruckProlog']);
    fakeTruckService.updateTruck.and.returnValue(Promise.resolve({status: 200}));
    fakeTruckService.getTruck.and.returnValue(Promise.resolve({truckID: "1", tare: 1, capacity: 1, maxBatteryCapacity: 1, autonomy: 1, fastChargeTime: 1}));

    TestBed.overrideProvider(TruckService, {useValue: fakeTruckService});
    fixture = TestBed.createComponent(EditTruckComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
    component.ngOnInit();
    
    dialogFixture = TestBed.createComponent(EditTruckComponentDialog);
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


  it('component finishes on Submit', async () => {

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


 
  it('should call onSubmit with truckID', async () => {
    spyOn(component, 'onSubmit');
    component.formEditTruck.controls['truckID'].setValue('truckID');
    await component.onSubmit();
    expect(component.onSubmit).toHaveBeenCalled();
  });


  it('should call onSubmit with error', async () => {
    fakeTruckService.updateTruck.and.returnValue(Promise.resolve({status: 500}));
    await component.onSubmit();
  });




});

describe('EditTruckComponent with null activated route', () => {
  let component: EditTruckComponent;
  let fixture: ComponentFixture<EditTruckComponent>;
  let dialogComponent: EditTruckComponentDialog;
  let dialogFixture: ComponentFixture<EditTruckComponentDialog>;
  let fakeTruckService: any;
  const dialogMock = {
    close: () => { }
  };

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ EditTruckComponent, EditTruckComponentDialog ],
      imports: [MatDialogModule,FormsModule,ReactiveFormsModule, BrowserAnimationsModule,MatCardModule,MatFormFieldModule,MatInputModule, RouterTestingModule.withRoutes(
        [{path: 'Logistics/Home/FleetManager', redirectTo: ''}]
      )],
      providers: [TruckService, {provide: MatDialogRef, useValue: dialogMock},{ provide: MAT_DIALOG_DATA, useValue: {} },{provide: ActivatedRoute,
        useValue: {
          snapshot: {
            paramMap: {
              get: () => null
            }
          }
        }}]
    })
    .compileComponents();

    fakeTruckService = jasmine.createSpyObj('TruckService', ['updateTruck', 'getTruck']);
    fakeTruckService.updateTruck.and.returnValue(Promise.resolve({status: 200}));
    fakeTruckService.getTruck.and.returnValue(Promise.resolve({truckID: "1", tare: 1, capacity: 1, maxBatteryCapacity: 1, autonomy: 1, fastChargeTime: 1}));

    TestBed.overrideProvider(TruckService, {useValue: fakeTruckService});
    fixture = TestBed.createComponent(EditTruckComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
    component.ngOnInit();
    
    dialogFixture = TestBed.createComponent(EditTruckComponentDialog);
    dialogComponent = dialogFixture.componentInstance;
    dialogFixture.detectChanges();
    dialogComponent.ngOnInit();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

});


describe('TruckService', () => {
  let service: TruckService;

  beforeEach(() => {
    TestBed.configureTestingModule({});
    service = TestBed.inject(TruckService);
    


  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });

   it('should get a truck', async () => {
    const response = {
      "truckID": "test",
      "tare": 1,
      "capacity": 1,
      "maxBatteryCapacity": 1,
      "autonomy": 1,
      "fastChargeTime": 1,
      json () {
        return this;
      }
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const truck = await service.getTruck('test');
    expect(fetchSpy).toHaveBeenCalled();
    expect(truck).toEqual(response);
  });


  it('should create a truck', async () => {
    const response = {
      "status": 201,
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const status = await service.createTruck('test');
    expect(fetchSpy).toHaveBeenCalled();
    expect(status.status).toEqual(201);
  });

  it('should create a truck prolog', async () => {
    const response = {
      "status": 201,
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const status = await service.createTruckProlog('test');
    expect(fetchSpy).toHaveBeenCalled();
    expect(status.status).toEqual(201);

  });

  it('should update a truck', async () => {
    const response = {
      "status": 200,
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const status = await service.updateTruck('test');
    expect(fetchSpy).toHaveBeenCalled();
    expect(status.status).toEqual(200);
  });

  it('should update a truck prolog', async () => {
    const response = {
      "status": 200,
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const status = await service.updateTruckProlog('test');
    expect(fetchSpy).toHaveBeenCalled();
    expect(status.status).toEqual(200);
  });

  it('should delete a truck', async () => {
    const response = {
      "status": 200,
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const status = await service.deleteTruck('test');
    expect(fetchSpy).toHaveBeenCalled();
    expect(status.status).toEqual(200);
  });

  it('should delete a truck prolog', async () => {
    const response = {
      "status": 200,
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const status = await service.deleteTruckProlog('test');
    expect(fetchSpy).toHaveBeenCalled();
    expect(status.status).toEqual(200);
  });

  it('should get all trucks', async () => {
    const response = {
      "trucks": [
        {
          "truckID": "test",
          "tare": 1,
          "capacity": 1,
          "maxBatteryCapacity": 1,
          "autonomy": 1,
          "fastChargeTime": 1,
        }
      ],
      json () {
        return this;
      }
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const trucks = await service.getAllTruck();
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
