import { ComponentFixture,TestBed } from "@angular/core/testing";
import { FormsModule, ReactiveFormsModule } from "@angular/forms";
import { MatDialogModule } from "@angular/material/dialog";
import { LoginService } from "src/app/Services/LoginService/login.service";
import { PackagingService } from "src/app/Services/PackageService/package.service";
import { PackageListComponent } from "./package-list.component";

describe('PackageListComponent', () => {
    let component: PackageListComponent;
    let fixture: ComponentFixture<PackageListComponent>;
    let fakeLoginService: LoginService;
    let fakePackageService: PackagingService;
    beforeEach(async () => {
        await TestBed.configureTestingModule({
            declarations: [PackageListComponent ],
            imports: [MatDialogModule,FormsModule,ReactiveFormsModule]
        }).compileComponents();
        fakeLoginService = TestBed.inject(LoginService);
        fakePackageService = TestBed.inject(PackagingService);
        fixture = TestBed.createComponent(PackageListComponent);
        component = fixture.componentInstance;
        fixture.detectChanges();
    });

    it('should be authenticated with admin role', async () => {

        const fetchSpy = spyOn<any>(fakeLoginService, 'getRole').and.returnValue(Promise.resolve("admin"));
        const response = await component.isAuthenticated();
        expect(response).toBeTrue();
        expect(fetchSpy).toHaveBeenCalled();
    
      });

    //   it('should create', () => {
    //     const mockPackageList = [
    //         { id: 1, truckId: 'abc', deliveryId: 'def', positionX: 0, positionY: 0, positionZ: 0 },
    //         { id: 2, truckId: 'ghi', deliveryId: 'jkl', positionX: 1, positionY: 1, positionZ: 1 }
    //       ];

    //     const fetchSpy = spyOn<any>(component, 'isAuthenticated').and.returnValue(Promise.resolve(true));
    //     const fetchSpy2 = spyOn<any>(fakePackageService, 'getPackage').and.returnValue(Promise.resolve(mockPackageList));
    //     fixture.detectChanges();
    //     component.ngOnInit();
    //     expect(fetchSpy).toHaveBeenCalled();
    //     expect(fetchSpy2).toHaveBeenCalled();
    //     expect(component.packageList).toEqual(mockPackageList);
    //     expect(component.originalPackageList).toEqual(mockPackageList);
    //     expect(component.filteredIDList).toEqual(mockPackageList);
    // });
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

