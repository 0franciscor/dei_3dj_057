import { ComponentFixture,TestBed } from "@angular/core/testing";
import { FormBuilder, FormsModule, ReactiveFormsModule } from "@angular/forms";
import { MatCardModule } from "@angular/material/card";
import { MatDialogModule, MatDialogRef, MAT_DIALOG_DATA } from "@angular/material/dialog";
import { MatFormFieldModule } from "@angular/material/form-field";
import { MatInputModule } from "@angular/material/input";
import { BrowserAnimationsModule } from "@angular/platform-browser/animations";
import { LoginService } from "src/app/Services/LoginService/login.service";
import { PackagingService } from "src/app/Services/PackageService/package.service";
import { TruckService } from "src/app/Services/TruckService/truck.service";
import { CreatePackageComponent } from "./create-package.component";


describe('CreatePackageComponent', () => {
    let component: CreatePackageComponent;
    let fixture: ComponentFixture<CreatePackageComponent>;
    let fakeLoginService: LoginService;
    let fakePackageService: PackagingService;
    const dialogMock = {
        close: () => { }
        };
    beforeEach(async () => {
        await TestBed.configureTestingModule({
            declarations: [CreatePackageComponent ],
            imports: [MatDialogModule,FormsModule,ReactiveFormsModule, BrowserAnimationsModule,MatCardModule,MatFormFieldModule,MatInputModule],
            providers: [TruckService, {provide: MatDialogRef, useValue: dialogMock},{ provide: MAT_DIALOG_DATA, useValue: {} },]
        }).compileComponents();
        fakeLoginService = TestBed.inject(LoginService);
        fakePackageService = TestBed.inject(PackagingService);
        fixture = TestBed.createComponent(CreatePackageComponent);
        component = fixture.componentInstance;
        fixture.detectChanges();
    });


    it('should be authenticated with admin role', async () => {

        const fetchSpy = spyOn<any>(fakeLoginService, 'getRole').and.returnValue(Promise.resolve("admin"));
        const response = await component.isAuthenticated();
        expect(response).toBeTrue();
        expect(fetchSpy).toHaveBeenCalled();
    
      });

    it('should create', () => {
        expect(component).toBeTruthy();
    });

    it('should submit', () => {
        component.formCreatePackage = new FormBuilder().group({
            packagingID: [''],
            truckID: [''],
            deliveryID: [''],
            xPosition: [''],
            yPosition: [''],
            zPosition: ['']
        });

        component.formCreatePackage.controls['packagingID'].setValue('1');
        component.formCreatePackage.controls['truckID'].setValue('1');
        component.formCreatePackage.controls['deliveryID'].setValue('1');
        component.formCreatePackage.controls['xPosition'].setValue('1');
        component.formCreatePackage.controls['yPosition'].setValue('1');
        component.formCreatePackage.controls['zPosition'].setValue('1');


        const fetchSpy = spyOn<any>(fakePackageService, 'createPackage').and.returnValue(Promise.resolve({status:200}));
        component.onSubmit();
        expect(fetchSpy).toHaveBeenCalled();
        expect(component).toBeTruthy();
    });

    it('should get packagingID', () => {
        component.formCreatePackage = new FormBuilder().group({
            packagingID: [''],
            truckID: [''],
            deliveryID: [''],
            xPosition: [''],
            yPosition: [''],
            zPosition: ['']
        });

        component.formCreatePackage.controls['packagingID'].setValue('1');

        expect(component.packagingID?.value).toEqual('1');        
        
    })
    
    it('should get truckID', () => {
        component.formCreatePackage = new FormBuilder().group({
            packagingID: [''],
            truckID: [''],
            deliveryID: [''],
            xPosition: [''],
            yPosition: [''],
            zPosition: ['']
        });

        component.formCreatePackage.controls['truckID'].setValue('1');

        expect(component.truckID?.value).toEqual('1');        
        
    })
    
    it('should get deliveryID', () => {
        component.formCreatePackage = new FormBuilder().group({
            packagingID: [''],
            truckID: [''],
            deliveryID: [''],
            xPosition: [''],
            yPosition: [''],
            zPosition: ['']
        });

        component.formCreatePackage.controls['deliveryID'].setValue('1');

        expect(component.deliveryID?.value).toEqual('1');        
        
    })
    
    it('should get xPosition', () => {
        component.formCreatePackage = new FormBuilder().group({
            packagingID: [''],
            truckID: [''],
            deliveryID: [''],
            xPosition: [''],
            yPosition: [''],
            zPosition: ['']
        });

        component.formCreatePackage.controls['xPosition'].setValue('1');

        expect(component.xPosition?.value).toEqual('1');        
        
    })

    it('should get yPosition', () => {
        component.formCreatePackage = new FormBuilder().group({
            packagingID: [''],
            truckID: [''],
            deliveryID: [''],
            xPosition: [''],
            yPosition: [''],
            zPosition: ['']
        });

        component.formCreatePackage.controls['yPosition'].setValue('1');

        expect(component.yPosition?.value).toEqual('1');        
        
    })
    
    it('should get zPosition', () => {
        component.formCreatePackage = new FormBuilder().group({
            packagingID: [''],
            truckID: [''],
            deliveryID: [''],
            xPosition: [''],
            yPosition: [''],
            zPosition: ['']
        });

        component.formCreatePackage.controls['zPosition'].setValue('1');

        expect(component.Position?.value).toEqual('1');        
        
    })

    
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
  
describe('PackagingService', () => {
let service: PackagingService;

beforeEach(() => {
    TestBed.configureTestingModule({});
    service = TestBed.inject(PackagingService);      
});

it('should be created', () => {
    expect(service).toBeTruthy();
})

it('should be created', () => {
    expect(service).toBeTruthy();
});

it('cookie with jwt', () => {
    spyOnProperty(document, 'cookie', 'get').and.returnValue('jwt=123');
    const cookie = service.getJwt();
    expect(cookie).toEqual('jwt=123');
    
});

it('should create a package', async () => {
    const response = {
        "status": 201,
        json() {
        return this;
        }
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const status = await service.createPackage('test');
    expect(fetchSpy).toHaveBeenCalled();
    expect(status.status).toEqual(201);
    service.urlOrigin = "https://azure:4200";
    await service.createPackage("test");
});

it('should get all packages', async () => {

    const response = {
        "status": 200,
        json() {
        return this;
        }
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const status = await service.getPackage();
    expect(fetchSpy).toHaveBeenCalled();
    expect(status.status).toEqual(200);
    service.urlOrigin = "https://azure:4200";
    await service.getPackage();


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