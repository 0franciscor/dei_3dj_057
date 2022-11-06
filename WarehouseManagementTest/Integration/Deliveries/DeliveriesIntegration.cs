using EletricGo.Controllers;
using EletricGo.Domain.Deliveries;
using Moq;
using EletricGo.Domain.Shared;
using Microsoft.AspNetCore.Mvc;

namespace WarehouseManagementTest.Integration.Deliveries
{
    [TestFixture]
    internal class DeliveriesIntegration
    {
        private readonly string? id = "testID";

        private readonly DateTime deliveryDate = new DateTime(2024, 12, 12);

        private readonly float loadTime = 10;

        private readonly float unloadTime = 20;

        private readonly string? destination = "testDestination";

        private readonly float deliveryMass = 30;
        

        //SETUP LISTS FOR GETALL()
        private List<Delivery> GetDeliveriesList()
        {
            var deliveryList = new List<Delivery>
            {
                new Delivery(id, new DeliveryDate(deliveryDate), new LoadTime(loadTime), new UnloadTime(unloadTime), new Destination(destination), new DeliveryMass(deliveryMass)),
                new Delivery("id2", new DeliveryDate(deliveryDate), new LoadTime(loadTime), new UnloadTime(unloadTime), new Destination(destination), new DeliveryMass(deliveryMass)),
                new Delivery("id3", new DeliveryDate(deliveryDate), new LoadTime(loadTime), new UnloadTime(unloadTime), new Destination(destination), new DeliveryMass(deliveryMass)),
                new Delivery("id4", new DeliveryDate(deliveryDate), new LoadTime(loadTime), new UnloadTime(unloadTime), new Destination(destination), new DeliveryMass(deliveryMass)),
                new Delivery("id5", new DeliveryDate(deliveryDate), new LoadTime(loadTime), new UnloadTime(unloadTime), new Destination(destination), new DeliveryMass(deliveryMass)),
            };

            return deliveryList;
        }

        [Test]
        public void DefineDriverServiceConstrutor()
        {
            var mockRepository = new Mock<IDeliveryRepository>();
            var mockUnit = new Mock<IUnitOfWork>();

            var service = new DeliveryService(mockUnit.Object, mockRepository.Object);

            Assert.That(service, Is.Not.Null);
        }

        [Test]
        public async Task GetAllTest()
        {
            var expectedList = GetDeliveriesList();

            var mockUnit = new Mock<IUnitOfWork>();
            var mockRepository = new Mock<IDeliveryRepository>();
            mockRepository.Setup(repo => repo.GetAll()).ReturnsAsync(expectedList);

            var deliveryService = new DeliveryService(mockUnit.Object, mockRepository.Object);
            var deliveryController = new DeliveryController(deliveryService);

            var resultList = await deliveryController.GetAll();

            for(int i = 0; i < expectedList.Count; i++)
            {
                Assert.AreEqual(expectedList[i].Id, resultList.Value[i].deliveryID);
            }
        }

        [Test]
        public async Task GetByIDTest()
        {
            var delivery = new Delivery(id, new DeliveryDate(deliveryDate), new LoadTime(loadTime), new UnloadTime(unloadTime), new Destination(destination), new DeliveryMass(deliveryMass));
            var deliveryExpected = delivery.toDeliveryDTO();
            var deliveryID = new DeliveryID(id);

            var mockUnit = new Mock<IUnitOfWork>();
            var mockRepository = new Mock<IDeliveryRepository>();
            mockRepository.Setup(repo => repo.GetByID(deliveryID)).ReturnsAsync(delivery);

            var deliveryService = new DeliveryService(mockUnit.Object, mockRepository.Object);

            var deliveryController = new DeliveryController(deliveryService);
            var aux = await deliveryController.GetByID(id);

            if (aux == null)
                Assert.Fail();
            else
            {
                var deliveryResult = ((DeliveryDTO)(aux.Result as OkObjectResult).Value);

                Assert.AreEqual(deliveryExpected.deliveryID, deliveryResult.deliveryID);
            }
        }

        [Test]
        public async Task GetByPeriod()
        {
            var delivery = new Delivery(id, new DeliveryDate(deliveryDate), new LoadTime(loadTime), new UnloadTime(unloadTime), new Destination(destination), new DeliveryMass(deliveryMass));
            var deliveryDTO = delivery.toDeliveryDTO();

            var dateTime1 = new DateTime(2024, 12, 10);
            var dateTime2 = new DateTime(2024, 12, 14);

            var listExpected = new List<DeliveryDTO>() { deliveryDTO };

            var mockUnit = new Mock<IUnitOfWork>();
            var mockRepository = new Mock<IDeliveryRepository>();
            mockRepository.Setup(repo => repo.GetByPeriod(dateTime1, dateTime2)).ReturnsAsync(new List<Delivery>() { delivery });

            var service = new DeliveryService(mockUnit.Object, mockRepository.Object);
            var controller = new DeliveryController(service);

            var aux = controller.GetByPeriod("10/12/2024,14/12/2024");

            if (aux == null)
                Assert.Fail();
            else
            {
                var listResult = ((List<DeliveryDTO>)(aux.Result.Result as OkObjectResult).Value);

                Assert.AreEqual(listExpected.First().deliveryID, listResult.First().deliveryID);
            }
        }


        [Test]
        public async Task PostTest()
        {
            var delivery = new Delivery(id, new DeliveryDate(deliveryDate), new LoadTime(loadTime), new UnloadTime(unloadTime), new Destination(destination), new DeliveryMass(deliveryMass));
            var deliveryExpected = delivery.toDeliveryDTO();

            var mockUnit = new Mock<IUnitOfWork>();
            var mockRepository = new Mock<IDeliveryRepository>();


            var mockRepo = new Mock<IDeliveryRepository>();
            mockRepo.Setup(repo => repo.Add(delivery)).ReturnsAsync(delivery);
            mockRepository.Setup(repo => repo.GetByID(new DeliveryID(id))).ReturnsAsync(null as Delivery);
            var mockUnitRepo = new Mock<IUnitOfWork>();
            mockUnitRepo.Setup(repo => repo.CommitAsync());

            var service = new DeliveryService(mockUnitRepo.Object, mockRepo.Object);
            var deliveryController = new DeliveryController(service);

            var aux = await deliveryController.Patch(deliveryExpected);

            if (aux == null)
                Assert.Fail();
            else
            {
                var deliveryResult = ((DeliveryDTO)(aux.Result as CreatedAtActionResult).Value);

                Assert.AreEqual(deliveryExpected.deliveryID, deliveryResult.deliveryID);
            }
        }

        [Test]
        public async Task PutTest()
        {
            var delivery = new Delivery(id, new DeliveryDate(deliveryDate), new LoadTime(loadTime), new UnloadTime(unloadTime), new Destination(destination), new DeliveryMass(deliveryMass));

            var preChangeDto = delivery.toDeliveryDTO();
            var postChangeDto = delivery.toDeliveryDTO();

            float newLoadTime = 3005;
            postChangeDto.loadTime = newLoadTime;

            var mockRepository = new Mock<IDeliveryRepository>();
            mockRepository.Setup(repo => repo.GetByID(new DeliveryID(id))).ReturnsAsync(delivery);
            
            var mockUnit = new Mock<IUnitOfWork>();
            mockUnit.Setup(repo => repo.CommitAsync()); //does not return, but the service method updates the object

            var deliveryService = new DeliveryService(mockUnit.Object, mockRepository.Object);

            var deliveryController = new DeliveryController(deliveryService);
            var aux = await deliveryController.Put(postChangeDto);

            if (aux == null)
                Assert.Fail();
            else
            {
                var deliveryResult = ((DeliveryDTO)(aux.Result as OkObjectResult).Value);

                Assert.AreEqual(newLoadTime, deliveryResult.loadTime);
            }
        }

        [Test]
        public async Task DeleteTest()
        {
            var delivery = new Delivery(id, new DeliveryDate(deliveryDate), new LoadTime(loadTime), new UnloadTime(unloadTime), new Destination(destination), new DeliveryMass(deliveryMass));
            var deliveryExpected = delivery.toDeliveryDTO();

            var mockRepository = new Mock<IDeliveryRepository>();
            mockRepository.Setup(repo => repo.GetByID(new DeliveryID(id))).ReturnsAsync(delivery);
            mockRepository.Setup(repo => repo.Delete(delivery));
            var mockUnit = new Mock<IUnitOfWork>();

            var deliveryService = new DeliveryService(mockUnit.Object, mockRepository.Object);
            var deliveryController = new DeliveryController(deliveryService);
            var aux = await deliveryController.Delete(id);

            if (aux == null)
                Assert.Fail();
            else
            {
                var deliveryResult = ((DeliveryDTO)(aux.Result as OkObjectResult).Value);

                Assert.AreEqual(deliveryResult.deliveryID, id);
            }
        }

    }
}
