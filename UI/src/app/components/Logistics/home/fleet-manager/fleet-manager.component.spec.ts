import { ComponentFixture, TestBed } from '@angular/core/testing';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MatCardModule } from '@angular/material/card';
import { MatDialogModule, MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { RouterTestingModule } from '@angular/router/testing';
import { LoginService } from 'src/app/Services/LoginService/login.service';
import { TruckService } from 'src/app/Services/TruckService/truck.service';

import { DeleteTruckComponentDialog, FleetManagerComponent } from './fleet-manager.component';

describe('FleetManagerComponent', () => {
  let component: FleetManagerComponent;
  let fixture: ComponentFixture<FleetManagerComponent>;
  let dialogComponent: DeleteTruckComponentDialog;
  let dialogFixture: ComponentFixture<DeleteTruckComponentDialog>;
  let fakeTruckService: TruckService;
  let fakeLoginService: LoginService;
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
      fastChargeTime: 1,
      active: true
    },
    {
      id: '2',
      truckID: 'test2',
      tare: 1,
      capacity: 1,
      maxBatteryCapacity: 1,
      autonomy: 1,
      fastChargeTime: 1,
      active: true
    }]

      


  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ FleetManagerComponent ],
      imports: [MatDialogModule,FormsModule,ReactiveFormsModule, BrowserAnimationsModule,MatCardModule,MatFormFieldModule,MatInputModule,RouterTestingModule.withRoutes(
        [
          {path: 'Logistics/Truck/CreateTruck', redirectTo: ''},
          {path: 'Logistics/Truck/EditTruck/test', redirectTo: ''}
      
        ])],
        providers: [{provide: MatDialogRef, useValue: dialogMock},{ provide: MAT_DIALOG_DATA, useValue: {} },]
    })
    .compileComponents();


    fakeTruckService = TestBed.inject(TruckService);
    fakeLoginService = TestBed.inject(LoginService);
    fixture = TestBed.createComponent(FleetManagerComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
    component.ngOnInit();

    dialogFixture = TestBed.createComponent(DeleteTruckComponentDialog);
    dialogComponent = dialogFixture.componentInstance;
    dialogFixture.detectChanges();
    dialogComponent.ngOnInit();
  });

  it('should be authenticated with admin role', async () => {

    const fetchSpy = spyOn<any>(fakeLoginService, 'getRole').and.returnValue(Promise.resolve("admin"));
    const response = await component.isAuthenticated();
    expect(response).toBeTrue();
    expect(fetchSpy).toHaveBeenCalled();

  });

  it('should create', () => {
    const fetchSpy = spyOn<any>(fakeLoginService, 'getRole').and.returnValue(Promise.resolve("admin"));
    component.ngOnInit();
    expect(fetchSpy).toHaveBeenCalled();
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

  // it('should deleteTruck', async () => {
    
  //   await component.toggleActiveTruck("test");
  //   expect(component).toBeTruthy();
  // });
  
  // it('shouldnt deleteTruck', async () => {
  //   fakeTruckService.toggleActiveTruck.and.returnValue(Promise.resolve({status: 409}));
  //   await component.toggleActiveTruck("test");
  //   expect(component).toBeTruthy();
  // });
 
  it('should toggle to see all', () => {
    component.toggleSeeAll();
    expect(component).toBeTruthy();
  });

  it('onOk', () => {

    dialogComponent.onOk();
    expect(dialogComponent).toBeTruthy();
  });

  it('onTruckSelected', () => {
    component.truckList = truckList;
    component.selectedTruckOption = "test1";
    component.onTruckSelected(new Event('click'));
    expect(component).toBeTruthy();
  });

  it('should toggle active truck', async () => {

    let truckList = [
      {
        id: '1',
        truckID: 'test1',
        tare: 1,
        capacity: 1,
        maxBatteryCapacity: 1,
        autonomy: 1,
        fastChargeTime: 1,
        active: true
      }]
    component.truckList = truckList;
    component.selectedTruck = truckList[0];
    const fetchSpy2 = spyOn<any>(fakeTruckService, 'toggleActiveTruck').and.returnValue(Promise.resolve({status: 200}));
    const fetchSpy3 = spyOn<any>(fakeTruckService, 'getAllTruck').and.returnValue(Promise.resolve(truckList));
    await component.toggleActiveTruck("test1");
    expect(fetchSpy2).toHaveBeenCalled();
    expect(fetchSpy3).toHaveBeenCalled();
  });
  
  it('should toggle inactive truck', async () => {

    let truckList = [
      {
        id: '1',
        truckID: 'test1',
        tare: 1,
        capacity: 1,
        maxBatteryCapacity: 1,
        autonomy: 1,
        fastChargeTime: 1,
        active: false
      }]
    component.truckList = truckList;
    component.selectedTruck = truckList[0];
    const fetchSpy2 = spyOn<any>(fakeTruckService, 'toggleActiveTruck').and.returnValue(Promise.resolve({status: 404}));
    const fetchSpy3 = spyOn<any>(fakeTruckService, 'getAllTruck').and.returnValue(Promise.resolve(truckList));
    await component.toggleActiveTruck("test1");
    expect(fetchSpy2).toHaveBeenCalled();
    expect(fetchSpy3).toHaveBeenCalled();
  });

  it('should delete truck', async () => {

    let truckList = [
      {
        id: '1',
        truckID: 'test1',
        tare: 1,
        capacity: 1,
        maxBatteryCapacity: 1,
        autonomy: 1,
        fastChargeTime: 1,
        active: false
      }]
    component.truckList = truckList;
    component.selectedTruck = truckList[0];
    component.selectedTruckOption = truckList[0];
    const fetchSpy2 = spyOn<any>(fakeTruckService, 'deleteTruck').and.returnValue(Promise.resolve({status: 200}));
    await component.deleteTruck("test1");
    expect(fetchSpy2).toHaveBeenCalled();

  });

  it('should fail delete truck', async () => {

    let truckList = [
      {
        id: '1',
        truckID: 'test1',
        tare: 1,
        capacity: 1,
        maxBatteryCapacity: 1,
        autonomy: 1,
        fastChargeTime: 1,
        active: false
      }]
    component.truckList = truckList;
    component.selectedTruck = truckList[0];
    component.selectedTruckOption = truckList[0];
    const fetchSpy2 = spyOn<any>(fakeTruckService, 'deleteTruck').and.returnValue(Promise.resolve({status: 404}));
    await component.deleteTruck("test1");
    expect(fetchSpy2).toHaveBeenCalled();

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

  it('cookie with jwt', () => {
    spyOnProperty(document, 'cookie', 'get').and.returnValue('jwt=123');
    const cookie = service.getJwt();
    expect(cookie).toEqual('jwt=123');
    
  });

  it('cookie without jwt', () => {
    spyOnProperty(document, 'cookie', 'get').and.returnValue('abc=123');
    const cookie = service.getJwt();
    expect(cookie).toEqual('jwt=');

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
    service.urlOrigin = "https://azure:4200";
    await service.getTruck("test");
  });


  it('should create a truck', async () => {
    const response = {
      "status": 201,
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const status = await service.createTruck('test');
    expect(fetchSpy).toHaveBeenCalled();
    expect(status.status).toEqual(201);
    service.urlOrigin = "https://azure:4200";
    await service.createTruck("test");
  });

  it('should create a truck prolog', async () => {
    const response = {
      "status": 201,
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const status = await service.createTruckProlog('test');
    expect(fetchSpy).toHaveBeenCalled();
    expect(status.status).toEqual(201);
    service.urlOrigin = "https://azure:4200";
    await service.createTruckProlog("test");

  });

  it('should update a truck', async () => {
    const response = {
      "status": 200,
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const status = await service.updateTruck('test');
    expect(fetchSpy).toHaveBeenCalled();
    expect(status.status).toEqual(200);
    service.urlOrigin = "https://azure:4200";
    await service.updateTruck("test");
  });

  it('should update a truck prolog', async () => {
    const response = {
      "status": 200,
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const status = await service.updateTruckProlog('test');
    expect(fetchSpy).toHaveBeenCalled();
    expect(status.status).toEqual(200);
    service.urlOrigin = "https://azure:4200";
    await service.updateTruckProlog("test");
  });

  it('should toggle a truck', async () => {
    const response = {
      "status": 200,
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const status = await service.toggleActiveTruck('test');
    expect(fetchSpy).toHaveBeenCalled();
    expect(status.status).toEqual(200);
    service.urlOrigin = "https://azure:4200";
    await service.toggleActiveTruck("test");
  });

  it('should delete a truck prolog', async () => {
    const response = {
      "status": 200,
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const status = await service.deleteTruck('test');
    expect(fetchSpy).toHaveBeenCalled();
    expect(status.status).toEqual(200);
    service.urlOrigin = "https://azure:4200";
    await service.deleteTruck("test");
  });

  it('should delete a truck prolog', async () => {
    const response = {
      "status": 200,
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const status = await service.deleteTruckProlog('test');
    // expect(fetchSpy).toHaveBeenCalled();
    expect(status.status).toEqual(200);
    service.urlOrigin = "https://azure:4200";
    await service.deleteTruckProlog("test");
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
    service.urlOrigin = "https://azure:4200";
    await service.getAllTruck();
  });

  it('should send a fetch without data', async () => {

    const status = await service.sendFetch('test', 'GET', null, "cookie");
    expect(status.status).toEqual(404);

  });

  it('should send a fetch with data', async () => {
    const status = await service.sendFetch('test', 'POST', "null", "cookie");
    expect(status.status).toEqual(404);
  });



});

describe('LoginService', () => {
  let service: LoginService;

  beforeEach(() => {
    TestBed.configureTestingModule({});
    service = TestBed.inject(LoginService);
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });

  it('get role with jwt cookie', async () => {
    const response = {
      "status": 200,
      json() {
        return {role:"admin"};
      }
    }
    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
    spyOnProperty(document, 'cookie', 'get').and.returnValue('jwt=123');
    const role = await service.getRole();
    expect(fetchSpy).toHaveBeenCalled();
    service.urlOrigin = "https://azure:4200";
    await service.getRole();
    
  });

  
  it('get role with null jwt cookie', async () => {
    const response = {
      "status": 401,
      json() {
        return {role:"admin"};
      }
    }
    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
    spyOnProperty(document, 'cookie', 'get').and.returnValue('');
    const role = await service.getRole();
    expect(fetchSpy).not.toHaveBeenCalled();
    service.urlOrigin = "https://azure:4200";
    await service.getRole();
    
  });

  it('get invalid role with jwt cookie', async () => {
    const response = {
      "status": 401,
      json() {
        return {role:"admin"};
      }
    }
    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
    spyOnProperty(document, 'cookie', 'get').and.returnValue('jwt=123');
    const role = await service.getRole();
    expect(fetchSpy).toHaveBeenCalled();
    service.urlOrigin = "https://azure:4200";
    await service.getRole();
    
  });

  it('should login', async () => {

    const response = {
      "status": 200,
      json() {
        return {token: "test"};
      }
    }
    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
    const login = await service.login("test");
    expect(fetchSpy).toHaveBeenCalled();
    service.urlOrigin = "https://azure:4200";
    await service.login("test");

  });

  it('should login with google', async () => {

    const response = {
      "status": 200,
      json() {
        return {token: "test"};
      }
    }
    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
    const login = await service.loginWithGoogle("test");
    expect(fetchSpy).toHaveBeenCalled();
    service.urlOrigin = "https://azure:4200";
    await service.loginWithGoogle("test");

  });




  it('should send a fetch without data', async () => {

    const status = await service.sendFetch('test', 'GET', null, "cookie");
    expect(status.status).toEqual(404);

  });

  it('should send a fetch with data', async () => {
    const status = await service.sendFetch('test', 'POST', "null", "cookie");
    expect(status.status).toEqual(404);
  });
});
