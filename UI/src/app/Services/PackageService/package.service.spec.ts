import { TestBed } from '@angular/core/testing';
import { PackagingService } from './package.service';


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