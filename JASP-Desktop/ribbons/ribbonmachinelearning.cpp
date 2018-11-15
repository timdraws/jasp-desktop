//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

#include "ribbonmachinelearning.h"
#include "ui_ribbonmachinelearning.h"

#include <QMenu>
///// additional Headers

RibbonMachineLearning::RibbonMachineLearning(QWidget *parent) :
	RibbonWidget(parent),
	ui(new Ui::RibbonMachineLearning)
{
	ui->setupUi(this);

	addRibbonButton(ui->buttonRegression);

	QMenu *menuRegression = new QMenu(this);
	menuRegression->addAction(QString("k nearest neighbors"), this, SLOT(itemSelected()))->setObjectName("MachineLearningknearestneighbors");
	menuRegression->addAction(QString("Random forest"), this, SLOT(itemSelected()))->setObjectName("MachineLearningRandomforest");
	menuRegression->addAction(QString("Boosting"), this, SLOT(itemSelected()))->setObjectName("MachineLearningBoosting");
	ui->buttonRegression->setMenu(menuRegression);

	addRibbonButton(ui->buttonClassification);

	QMenu *menuClassification = new QMenu(this);
	menuClassification->addAction(QString("k nearest neighbors"), this, SLOT(itemSelected()))->setObjectName("MachineLearningknearestneighbors");
	menuClassification->addAction(QString("Random forest"), this, SLOT(itemSelected()))->setObjectName("MachineLearningRandomforest");
	menuClassification->addAction(QString("Boosting"), this, SLOT(itemSelected()))->setObjectName("MachineLearningBoosting");
	ui->buttonClassification->setMenu(menuClassification);

	addRibbonButton(ui->buttonClustering);

	QMenu *menuClustering = new QMenu(this);
	menuClustering->addAction(QString("k nearest neighbors"), this, SLOT(itemSelected()))->setObjectName("MachineLearningknearestneighbors");
	menuClustering->addAction(QString("Random forest"), this, SLOT(itemSelected()))->setObjectName("MachineLearningRandomforest");
	ui->buttonClustering->setMenu(menuClustering);

///// Ribbon Buttons and Menu

}

RibbonMachineLearning::~RibbonMachineLearning()
{
	delete ui;
}
