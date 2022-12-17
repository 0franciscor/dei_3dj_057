import { ComponentFixture, TestBed } from '@angular/core/testing';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MatCardModule } from '@angular/material/card';
import { MatDialogModule, MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { RouterTestingModule } from '@angular/router/testing';
import { TruckService } from 'src/app/Services/TruckService/truck.service';

import { DeleteTruckComponentDialog, FleetManagerComponent } from './fleet-manager.component';

describe('FleetManagerComponent', () => {
  let component: FleetManagerComponent;
  let fixture: ComponentFixture<FleetManagerComponent>;
  let dialogComponent: DeleteTruckComponentDialog;
  let dialogFixture: ComponentFixture<DeleteTruckComponentDialog>;
  let fakeTruckService: any;
  const dialogMock = {
    close: () => { }
  };

  const truckList = [
    {
      id: '1',
      truckID: 'test1',
      tare: 1,
      capacity: 1,
      maxBatteryCapacity: 1,
      autonomy: 1,
      fastChargeTime: 1
    },
    {
      id: '2',
      truckID: 'test2',
      tare: 1,
      capacity: 1,
      maxBatteryCapacity: 1,
      autonomy: 1,
      fastChargeTime: 1
    }]

      


  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ FleetManagerComponent ],
      imports: [MatDialogModule,FormsModule,ReactiveFormsModule, BrowserAnimationsModule,MatCardModule,MatFormFieldModule,MatInputModule,RouterTestingModule.withRoutes(
        [
          {path: 'Logistics/Truck/CreateTruck', redirectTo: ''},
          {path: 'Logistics/Truck/EditTruck/test', redirectTo: ''}
      
        ])],
        providers: [TruckService, {provide: MatDialogRef, useValue: dialogMock},{ provide: MAT_DIALOG_DATA, useValue: {} },]
    })
    .compileComponents();

    fakeTruckService = jasmine.createSpyObj('TruckService', ['getAllTruck','toggleActiveTruck']);
    fakeTruckService.getAllTruck.and.returnValue(Promise.resolve(truckList));
    fakeTruckService.toggleActiveTruck.and.returnValue(Promise.resolve({status: 200}));

    TestBed.overrideProvider(TruckService, {useValue: fakeTruckService});

    fixture = TestBed.createComponent(FleetManagerComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();

    dialogFixture = TestBed.createComponent(DeleteTruckComponentDialog);
    dialogComponent = dialogFixture.componentInstance;
    dialogFixture.detectChanges();
    dialogComponent.ngOnInit();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should go to create truck', () => {
    component.goToCreateTruck();
    expect(component).toBeTruthy();
  });

  it('should go to edit truck', () => {
    component.goToEditTruck("test");
    expect(component).toBeTruthy();
  });

  it('should deleteTruck', async () => {

    await component.toggleActiveTruck("test");
    expect(component).toBeTruthy();
  });
  
  it('shouldnt deleteTruck', async () => {
    fakeTruckService.toggleActiveTruck.and.returnValue(Promise.resolve({status: 409}));
    await component.toggleActiveTruck("test");
    expect(component).toBeTruthy();
  });
 
  it('should toggle to see all', () => {
    component.toggleSeeAll();
    expect(component).toBeTruthy();
  });

  it('onOk', () => {
    dialogComponent.onOk();
    expect(dialogComponent).toBeTruthy();
  });

  it('onTruckSelected', () => {
    component.onTruckSelected(new Event('click'));
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

    const status = await service.toggleActiveTruck('test');
    expect(fetchSpy).toHaveBeenCalled();
    expect(status.status).toEqual(200);
  });

  it('should delete a truck prolog', async () => {
    const response = {
      "status": 200,
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const status = await service.deleteTruckProlog('test');
    // expect(fetchSpy).toHaveBeenCalled();
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

    const status = await service.sendFetch('test', 'GET', null,"cookie");
    expect(status.status).toEqual(404);
  });

  it('should send a fetch with data', async () => {
    const status = await service.sendFetch('test', 'POST', "null","cookie");
    expect(status.status).toEqual(404);
  });

});
