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
});