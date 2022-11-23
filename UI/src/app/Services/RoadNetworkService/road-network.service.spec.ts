import { TestBed } from '@angular/core/testing';

import { RoadNetworkService } from './road-network.service';

describe('RoadNetworkService', () => {
  let service: RoadNetworkService;

  beforeEach(() => {
    TestBed.configureTestingModule({});
    service = TestBed.inject(RoadNetworkService);
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });
});
