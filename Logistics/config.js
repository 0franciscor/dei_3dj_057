import dotenv from 'dotenv';

export default {
  /**
   * API configs
   */
  api: {
    prefix: '/api',
  },

  controllers: {
    truck: {
      name: "TruckController",
      path: "../controllers/truckController"
    }
  },

  repos: {
    truck: {
      name: "TruckRepo",
      path: "../repos/truckRepo"
    }
  },

  services: {
    truck: {
      name: "TruckService",
      path: "../services/truckService"
    }
  },
};
