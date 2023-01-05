import { TestBed } from '@angular/core/testing';
import { PathService } from './path.service'; 

describe('PathService', ()=>{
    let service: PathService;

    beforeEach(() => {
        TestBed.configureTestingModule({});
        service = TestBed.inject(PathService);
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

      it('should get all paths', async () => {
        const response = {
          "path": [
            {
              "id": "TH1",
              "address": "Rua António Bernardino,47,4535-334,Porto",
              "altitude": 250,
              "latitude": "40.9321º N",
              "longitude": "8.2451º W",
              "designation": "Arouca",
              "city": "1",
            },
            {
              "id": "TH2",
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
          },
          status: 200
        };

        const warehouses = {
          startWHId: "WH1",
          destinationWHId: "WH2"
        }
        const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
        const paths = await service.getAllPaths(warehouses);
        expect(fetchSpy).toHaveBeenCalled();
        expect(paths).toEqual(response);
        service.urlOrigin = "https://azure:4200";
        await service.getAllPaths(warehouses);

      });

      it('should get all paths with undefined', async () => {
        const response = {
          "path": [
            {
              "id": "TH1",
              "address": "Rua António Bernardino,47,4535-334,Porto",
              "altitude": 250,
              "latitude": "40.9321º N",
              "longitude": "8.2451º W",
              "designation": "Arouca",
              "city": "1",
            },
            {
              "id": "TH2",
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
          },
          status: 404
        };

        const warehouses = {
          startWHId: "",
          destinationWHId: ""
        }
        const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
        const paths = await service.getAllPaths(warehouses);
        expect(fetchSpy).toHaveBeenCalled();
        expect(paths).toEqual(response);
        service.urlOrigin = "https://azure:4200";
        await service.getAllPaths(warehouses);

      });

      it('should create a path', async () => {

        const response = {
          "path": {
            "id": "TH1",
            "address": "Rua António Bernardino,47,4535-334,Porto",
            "altitude": 250,
            "latitude": "40.9321º N",
            "longitude": "8.2451º W",
            "designation": "Arouca",
            "city": "1",
          },
          json () {
            return this;
          },
          status: 200
        };

        const path = {
          "id": "TH1",
          "address": "Rua António Bernardino,47,4535-334,Porto",
          "altitude": 250,
          "latitude": "40.9321º N",
          "longitude": "8.2451º W",
          "designation": "Arouca",
          "city": "1",
        }

        const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
        const paths = await service.createPath(path);
        expect(fetchSpy).toHaveBeenCalled();
        service.urlOrigin = "https://azure:4200";
        await service.createPath(path);


      });

      it('should create a path in prolog', async () => {

        const response = {
          "path": {
            "id": "TH1",
            "address": "Rua António Bernardino,47,4535-334,Porto",
            "altitude": 250,
            "latitude": "40.9321º N",
            "longitude": "8.2451º W",
            "designation": "Arouca",
            "city": "1",
          },
          json () {
            return this;
          },
          status: 200
        };

        const path = {
          "id": "TH1",
          "address": "Rua António Bernardino,47,4535-334,Porto",
          "altitude": 250,
          "latitude": "40.9321º N",
          "longitude": "8.2451º W",
          "designation": "Arouca",
          "city": "1",
        }

        const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
        const paths = await service.createPathProlog(path);
        expect(fetchSpy).toHaveBeenCalled();
        service.urlOrigin = "https://azure:4200";
        await service.createPathProlog(path);


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