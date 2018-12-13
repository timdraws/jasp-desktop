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

#include "mainwindow.h"
#include "ui_mainwindow.h"

#include "analysisforms/Common/ancovabayesianform.h"
#include "analysisforms/Common/ancovaform.h"
#include "analysisforms/Common/ancovamultivariateform.h"
#include "analysisforms/Common/anovabayesianform.h"
#include "analysisforms/Common/anovaform.h"
#include "analysisforms/Common/anovamultivariateform.h"
#include "analysisforms/Common/anovaonewayform.h"
#include "analysisforms/Common/anovarepeatedmeasuresbayesianform.h"
#include "analysisforms/Common/anovarepeatedmeasuresform.h"
#include "analysisforms/Common/binomialtestbayesianform.h"
#include "analysisforms/Common/binomialtestform.h"
#include "analysisforms/Common/contingencytablesbayesianform.h"
#include "analysisforms/Common/contingencytablesform.h"
#include "analysisforms/Common/correlationbayesianform.h"
#include "analysisforms/Common/correlationbayesianpairsform.h"
#include "analysisforms/Common/correlationform.h"
#include "analysisforms/Common/correlationpartialform.h"
#include "analysisforms/Common/descriptivesform.h"
#include "analysisforms/Common/exploratoryfactoranalysisform.h"
#include "analysisforms/Common/principalcomponentanalysisform.h"
#include "analysisforms/Common/regressionlinearbayesianform.h"
#include "analysisforms/Common/regressionlinearform.h"
#include "analysisforms/Common/regressionlogisticform.h"
#include "analysisforms/Common/regressionloglinearbayesianform.h"
#include "analysisforms/Common/regressionloglinearform.h"
#include "analysisforms/Common/reliabilityanalysisform.h"
#include "analysisforms/Common/ttestbayesianindependentsamplesform.h"
#include "analysisforms/Common/ttestbayesianonesampleform.h"
#include "analysisforms/Common/ttestbayesianpairedsamplesform.h"
#include "analysisforms/Common/ttestindependentsamplesform.h"
#include "analysisforms/Common/ttestonesampleform.h"
#include "analysisforms/Common/ttestpairedsamplesform.h"
#include "analysisforms/Common/multinomialtestform.h"

#include "analysisforms/SummaryStatistics/summarystatsbinomialtestbayesianform.h"
#include "analysisforms/SummaryStatistics/summarystatscorrelationbayesianpairsform.h"
#include "analysisforms/SummaryStatistics/summarystatsregressionlinearbayesianform.h"
#include "analysisforms/SummaryStatistics/summarystatsttestbayesianindependentsamplesform.h"
#include "analysisforms/SummaryStatistics/summarystatsttestbayesianonesampleform.h"
#include "analysisforms/SummaryStatistics/summarystatsttestbayesianpairedsamplesform.h"

#include "analysisforms/SEM/semsimpleform.h"

#include "analysisforms/ReinforcementLearning/reinforcementlearningr11tlearningform.h"

#include "analysisforms/Network/networkanalysisform.h"

#include "analysisforms/MetaAnalysis/classicalmetaanalysisform.h"

#include "analysisforms/MachineLearning/mlregressionrandomforestform.h"
#include "analysisforms/MachineLearning/mlregressionknnform.h"
#include "analysisforms/MachineLearning/mlregressionboostingform.h"
#include "analysisforms/MachineLearning/mlclassificationrandomforestform.h"
#include "analysisforms/MachineLearning/mlclassificationknnform.h"
#include "analysisforms/MachineLearning/mlclassificationboostingform.h"
#include "analysisforms/MachineLearning/mlclusteringrandomforestform.h"
#include "analysisforms/MachineLearning/mlclusteringkmeansform.h"

///// 1-analyses headers

#include <QDebug>
#include <QFile>
#include <QFileInfo>
#include <QToolTip>
#include <QMessageBox>
#include <QStringBuilder>
#include <QDropEvent>
#include <QShortcut>
#include <QDesktopWidget>
#include <QTabBar>
#include <QMenuBar>
#include <QDir>
#include <QFileDialog>
#include <QDesktopServices>
#include <QQmlContext>
#include <QQuickItem>
#include <QScreen>

#include "analysisloader.h"

#include "qutils.h"
#include "appdirs.h"
#include "tempfiles.h"
#include "processinfo.h"
#include "appinfo.h"

#include "aboutdialog.h"
#include "preferencesdialog.h"
#include <boost/filesystem.hpp>
#include "dirs.h"
#include "qutils.h"
#include "column.h"
#include "sharedmemory.h"
#include "module.h"
#include "settings.h"

#include "options/optionvariablesgroups.h"
#include "datasetview.h"

#include "timers.h"
#include "resultstesting/compareresults.h"

using namespace std;

MainWindow::MainWindow(QApplication * application) : QMainWindow(NULL), ui(new Ui::MainWindow), _application(application)
{
	JASPTIMER_START(MainWindowConstructor);

	ui->setupUi(this);

	tempfiles_init(ProcessInfo::currentPID()); // needed here so that the LRNAM can be passed the session directory

	_resultsJsInterface		= new ResultsJsInterface(this);
	_package				= new DataSetPackage();
	_odm					= new OnlineDataManager(this);
	_tableModel				= new DataSetTableModel();
	_levelsTableModel		= new LevelsTableModel(this);
	_labelFilterGenerator	= new labelFilterGenerator(_package, this);
	_columnsModel			= new ColumnsModel(this);
	_analyses				= new Analyses();
	_engineSync				= new EngineSync(_analyses, _package, this);
	_computedColumnsModel	= new ComputedColumnsModel(_analyses, this);
	_filterModel			= new FilterModel(_package, this);

	StartOnlineDataManager();
	initQWidgetGUIParts();
	makeConnections();
	
	// Set the initial tab on Common.
	tabChanged(1);
	
	qmlRegisterType<DataSetView>("JASP", 1, 0, "DataSetView");
	loadQML();

	QString missingvaluestring = _settings.value("MissingValueList", "").toString();
	if (missingvaluestring != "")
		Utils::setEmptyValues(fromQstringToStdVector(missingvaluestring, "|"));

	JASPTIMER_FINISH(MainWindowConstructor);
}

void MainWindow::StartOnlineDataManager()
{
	_loader.moveToThread(&_loaderThread);
	_loaderThread.start();
	_loader.setOnlineDataManager(_odm);

	ui->backStage->setOnlineDataManager(_odm);
}

#define CONNECT_SHORTCUT(shortcut, method) connect(new QShortcut(QKeySequence(shortcut), this),	&QShortcut::activated,	this,	method);

Q_DECLARE_METATYPE(Column::ColumnType)

void MainWindow::makeConnections()
{
	_package->isModifiedChanged.connect(boost::bind(&MainWindow::packageChanged,		this,	_1));
	_package->dataChanged.connect(		boost::bind(&MainWindow::packageDataChanged,	this,	_1, _2, _3, _4, _5));
	_package->pauseEngines.connect(		boost::bind(&MainWindow::pauseEngines,			this));
	_package->resumeEngines.connect(	boost::bind(&MainWindow::resumeEngines,			this));

	CONNECT_SHORTCUT("Ctrl+S",		&MainWindow::saveKeysSelected);
	CONNECT_SHORTCUT("Ctrl+O",		&MainWindow::openKeysSelected);
	CONNECT_SHORTCUT("Ctrl+Y",		&MainWindow::syncKeysSelected);
	CONNECT_SHORTCUT("Ctrl+T",		&MainWindow::refreshKeysSelected);
	CONNECT_SHORTCUT("Ctrl++",		&MainWindow::zoomInKeysSelected);
	CONNECT_SHORTCUT("Ctrl+-",		&MainWindow::zoomOutKeysSelected);
	CONNECT_SHORTCUT("Ctrl+=",		&MainWindow::zoomEqualKeysSelected);

	connect(_levelsTableModel,		&LevelsTableModel::resizeLabelColumn,				this,					&MainWindow::resizeVariablesWindowLabelColumn				);
	connect(_levelsTableModel,		&LevelsTableModel::labelFilterChanged,				_labelFilterGenerator,	&labelFilterGenerator::labelFilterChanged					);
	connect(_levelsTableModel,		&LevelsTableModel::notifyColumnHasFilterChanged,	_tableModel,			&DataSetTableModel::notifyColumnFilterStatusChanged			);
	connect(_levelsTableModel,		&LevelsTableModel::refreshConnectedModels,			_tableModel,			&DataSetTableModel::refreshColumn							);
	connect(_levelsTableModel,		&LevelsTableModel::refreshConnectedModelsByName,	_computedColumnsModel,	&ComputedColumnsModel::checkForDependentColumnsToBeSentSlot	);

	connect(_tableModel,			&DataSetTableModel::dataSetChanged,					this,					&MainWindow::dataSetChanged									);
	connect(_tableModel,			&DataSetTableModel::allFiltersReset,				_labelFilterGenerator,	&labelFilterGenerator::labelFilterChanged					);
	connect(_tableModel,			&DataSetTableModel::allFiltersReset,				_levelsTableModel,		&LevelsTableModel::refresh,									Qt::QueuedConnection);
	connect(_tableModel,			&DataSetTableModel::modelReset,						_levelsTableModel,		&LevelsTableModel::refresh,									Qt::QueuedConnection);
	connect(_tableModel,			&DataSetTableModel::headerDataChanged,				_columnsModel,			&ColumnsModel::datasetHeaderDataChanged						);
	connect(_tableModel,			&DataSetTableModel::modelReset,						_columnsModel,			&ColumnsModel::refresh										);
	connect(_tableModel,			&DataSetTableModel::columnDataTypeChanged,			_computedColumnsModel,	&ComputedColumnsModel::recomputeColumn						);

	connect(_engineSync,			&EngineSync::computeColumnSucceeded,				_computedColumnsModel,	&ComputedColumnsModel::computeColumnSucceeded				);
	connect(_engineSync,			&EngineSync::computeColumnFailed,					_computedColumnsModel,	&ComputedColumnsModel::computeColumnFailed					);
	connect(_engineSync,			&EngineSync::processNewFilterResult,				_filterModel,			&FilterModel::processFilterResult							);
	connect(_engineSync,			&EngineSync::processFilterErrorMsg,					_filterModel,			&FilterModel::processFilterErrorMsg							);

	qRegisterMetaType<Column::ColumnType>();

	connect(_computedColumnsModel,	&ComputedColumnsModel::refreshColumn,				_tableModel,			&DataSetTableModel::refreshColumn,							Qt::QueuedConnection);
	connect(_computedColumnsModel,	&ComputedColumnsModel::headerDataChanged,			_tableModel,			&DataSetTableModel::headerDataChanged,						Qt::QueuedConnection);
	connect(_computedColumnsModel,	&ComputedColumnsModel::sendComputeCode,				_engineSync,			&EngineSync::computeColumn,									Qt::QueuedConnection);
	connect(_computedColumnsModel,	&ComputedColumnsModel::refreshColumn,				_levelsTableModel,		&LevelsTableModel::refreshColumn,							Qt::QueuedConnection);
	connect(_computedColumnsModel,	&ComputedColumnsModel::dataSetChanged,				_tableModel,			&DataSetTableModel::dataSetChanged							);
	connect(_computedColumnsModel,	&ComputedColumnsModel::refreshData,					_tableModel,			&DataSetTableModel::refresh,								Qt::QueuedConnection);
	connect(_computedColumnsModel,	&ComputedColumnsModel::refreshData,					this,					&MainWindow::updateShownVariablesModel						);
	connect(_computedColumnsModel,	&ComputedColumnsModel::showAnalysisForm,			this,					&MainWindow::showForm										);

	connect(this,					&MainWindow::ppiChanged,							_engineSync,			&EngineSync::ppiChanged										);
	connect(this,					&MainWindow::imageBackgroundChanged,				_engineSync,			&EngineSync::imageBackgroundChanged							);

	connect(_analyses,				&Analyses::analysisResultsChanged,					this,					&MainWindow::analysisResultsChangedHandler					);
	connect(_analyses,				&Analyses::analysisImageSaved,						this,					&MainWindow::analysisImageSavedHandler						);
	connect(_analyses,				&Analyses::analysisAdded,							ui->backStage,			&BackStageWidget::analysisAdded								);
	connect(_analyses,				&Analyses::analysisImageEdited,						_resultsJsInterface,	&ResultsJsInterface::analysisImageEditedHandler				);
	connect(_analyses,				&Analyses::requestComputedColumnCreation,			_computedColumnsModel,	&ComputedColumnsModel::requestComputedColumnCreation,		Qt::UniqueConnection);
	connect(_analyses,				&Analyses::requestComputedColumnDestruction,		_computedColumnsModel,	&ComputedColumnsModel::requestComputedColumnDestruction,	Qt::UniqueConnection);

	connect(ui->backStage,			&BackStageWidget::exportSelected,					_resultsJsInterface,	&ResultsJsInterface::exportSelected							);
	connect(ui->backStage,			&BackStageWidget::dataSetIORequest,					this,					&MainWindow::dataSetIORequest								);

	connect(_odm,					&OnlineDataManager::progress,						this,					&MainWindow::setProgressStatus,								Qt::QueuedConnection);
	connect(&_loader,				&AsyncLoader::progress,								this,					&MainWindow::setProgressStatus								);
	connect(_engineSync,			&EngineSync::engineTerminated,						this,					&MainWindow::fatalError										);
	connect(_okButton,				&QPushButton::clicked,								this,					&MainWindow::analysisOKed									);
	connect(_runButton,				&QPushButton::clicked,								this,					&MainWindow::analysisRunned									);
	connect(ui->splitter,			&QSplitter::splitterMoved,							this,					&MainWindow::splitterMovedHandler							);
	connect(ui->webViewHelp,		&CustomWebEngineView::loadFinished,					this,					&MainWindow::helpFirstLoaded								);

	connect(ui->tabBar,				&TabBar::currentChanged,							this,					&MainWindow::tabChanged										);
	connect(ui->tabBar,				&TabBar::helpToggled,								this,					&MainWindow::helpToggled									);
	connect(ui->tabBar,				&TabBar::dataAutoSynchronizationChanged,			ui->backStage,			&BackStageWidget::dataAutoSynchronizationChanged			);
	connect(ui->tabBar,				&TabBar::setExactPValuesHandler,					_resultsJsInterface,	&ResultsJsInterface::setExactPValuesHandler					);
	connect(ui->tabBar,				&TabBar::setFixDecimalsHandler,						_resultsJsInterface,	&ResultsJsInterface::setFixDecimalsHandler					);
	connect(ui->tabBar,				&TabBar::emptyValuesChangedHandler,					this,					&MainWindow::emptyValuesChangedHandler						);
	connect(ui->tabBar,				&TabBar::useDefaultPPIHandler,						_resultsJsInterface,	&ResultsJsInterface::getDefaultPPI							);

	connect(_filterModel,			&FilterModel::refreshAllAnalyses,					this,					&MainWindow::refreshAllAnalyses								);
	connect(_filterModel,			&FilterModel::updateColumnsUsedInConstructedFilter, _tableModel,			&DataSetTableModel::setColumnsUsedInEasyFilter				);
	connect(_filterModel,			&FilterModel::filterUpdated,						_tableModel,			&DataSetTableModel::refresh									);
	connect(_filterModel,			&FilterModel::sendFilter,							_engineSync,			&EngineSync::sendFilter										);

	connect(_filterModel,			&FilterModel::updateGeneratedFilterWithR,			_labelFilterGenerator,	&labelFilterGenerator::easyFilterConstructorRCodeChanged	);
	connect(_labelFilterGenerator,	&labelFilterGenerator::setGeneratedFilter,			_filterModel,			&FilterModel::setGeneratedFilter							);
	connect(_engineSync,			&EngineSync::computeColumnSucceeded,				_filterModel,			&FilterModel::computeColumnSucceeded						);

	connectRibbonButton(ui->ribbonAnalysis);
	connectRibbonButton(ui->ribbonSEM);
	connectRibbonButton(ui->ribbonReinforcementLearning);
	connectRibbonButton(ui->ribbonSummaryStatistics);
	connectRibbonButton(ui->ribbonMetaAnalysis);
	connectRibbonButton(ui->ribbonNetworkAnalysis);
    connectRibbonButton(ui->ribbonMachineLearning);

}

void MainWindow::initQWidgetGUIParts()
{
	JASPTIMER_START(MainWindow::initQWidgetGUIParts());
	updateMenuEnabledDisabledStatus();

	ui->splitter->setSizes(QList<int>({575}));

	ui->tabBar->init(this);

#ifdef __APPLE__
	_scrollbarWidth = 3;
#else
	_scrollbarWidth = qApp->style()->pixelMetric(QStyle::PM_ScrollBarExtent);
#endif

	_buttonPanel		= new QWidget(ui->panel_2_Options);
	_buttonPanelLayout	= new QVBoxLayout(_buttonPanel);
	_buttonPanelLayout->setSpacing(6);
	_buttonPanelLayout->setContentsMargins(0, _buttonPanelLayout->contentsMargins().top(), _buttonPanelLayout->contentsMargins().right(), 0);

	_buttonPanel->setLayout(_buttonPanelLayout);

	_runButton	= new QPushButton(QString("Run"), _buttonPanel);
	_okButton	= new QPushButton(QString("OK"), _buttonPanel);
	_okButton->setDefault(true);

	QMenuBar *_mMenuBar = new QMenuBar(0);
	QMenu *aboutMenu	= _mMenuBar->addMenu("JASP");
	aboutMenu->addAction("About",ui->tabBar,SLOT(showAbout()));
	_mMenuBar->addMenu(aboutMenu);

	_buttonPanelLayout->addWidget(_okButton);
	_buttonPanelLayout->addWidget(_runButton);
	_buttonPanelLayout->addStretch();

	_buttonPanel->resize(_buttonPanel->sizeHint());
	_buttonPanel->move(ui->panel_2_Options->width() - _buttonPanel->width() - _scrollbarWidth, 0);

	_tableViewWidthBeforeOptionsMadeVisible = -1;

	ui->webViewHelp->load(QUrl::fromLocalFile(AppDirs::help() + "/index.html"));

	ui->panel_4_Help->hide();

	setAcceptDrops(true);

	ui->panel_1_Data->hide();
	ui->panel_2_Options->hide();

	JASPTIMER_FINISH(MainWindow::initQWidgetGUIParts());
}

void MainWindow::loadQML()
{
	ui->quickWidget_Data->rootContext()->setContextProperty("mainWindow",				this);
	ui->quickWidget_Data->rootContext()->setContextProperty("dataSetModel",				_tableModel);
	ui->quickWidget_Data->rootContext()->setContextProperty("levelsTableModel",			_levelsTableModel);
	ui->quickWidget_Data->rootContext()->setContextProperty("columnsModel",				_columnsModel);
	ui->quickWidget_Data->rootContext()->setContextProperty("computedColumnsInterface",	_computedColumnsModel);
	ui->quickWidget_Data->rootContext()->setContextProperty("engineSync",				_engineSync);
	ui->quickWidget_Data->rootContext()->setContextProperty("filterModel",				_filterModel);
	ui->quickWidget_Data->rootContext()->setContextProperty("baseBlockDim",				20);
	ui->quickWidget_Data->rootContext()->setContextProperty("baseFontSize",				16);
	ui->quickWidget_Data->rootContext()->setContextProperty("ppiScale",					Settings::value(Settings::UI_SCALE).toFloat());

	ui->quickWidget_Data->rootContext()->setContextProperty("columnTypeScale",			int(Column::ColumnType::ColumnTypeScale));
	ui->quickWidget_Data->rootContext()->setContextProperty("columnTypeOrdinal",		int(Column::ColumnType::ColumnTypeOrdinal));
	ui->quickWidget_Data->rootContext()->setContextProperty("columnTypeNominal",		int(Column::ColumnType::ColumnTypeNominal));
	ui->quickWidget_Data->rootContext()->setContextProperty("columnTypeNominalText",	int(Column::ColumnType::ColumnTypeNominalText));

	ui->quickWidget_Data->setSource(QUrl(QString("qrc:///qml/dataset.qml")));

	QObject * DataView				= ui->quickWidget_Data->rootObject()->findChild<QObject*>("dataSetTableView");
	QObject * levelsTableView		= ui->quickWidget_Data->rootObject()->findChild<QObject*>("levelsTableView");

	connect(DataView,				SIGNAL(dataTableDoubleClicked()),	this,					SLOT(startDataEditorHandler()));
	connect(levelsTableView,		SIGNAL(columnChanged(QString)),		this,					SLOT(refreshAnalysesUsingColumn(QString)));

	qmlProgressBar			= ui->quickWidget_Data->rootObject()->findChild<QObject*>("progressBarHolder");
}

void MainWindow::open(QString filepath)
{
	_openedUsingArgs = true;
	if (_resultsViewLoaded)
		ui->backStage->open(filepath);
	else
		_openOnLoadFilename = filepath;
}

MainWindow::~MainWindow()
{
	delete _engineSync;
	if (_package && _package->dataSet())
	{
		_loader.free(_package->dataSet());
		_package->reset();
	}
	delete ui;
}


void MainWindow::resizeEvent(QResizeEvent *event)
{
	QMainWindow::resizeEvent(event);
	adjustOptionsPanelWidth();
}

void MainWindow::dragEnterEvent(QDragEnterEvent *event)
{
	const QMimeData *data = event->mimeData();

	if (data->hasUrls())
	{
		QList<QUrl> urls = data->urls();
		QUrl first = urls.first();
		QFileInfo file(first.path());

		if (file.exists() && (file.completeSuffix() == "csv" || file.completeSuffix() == "jasp"))
			event->accept();
		else
			event->ignore();
	}
	else
	{
		event->ignore();
	}
}


void MainWindow::dropEvent(QDropEvent *event)
{
	const QMimeData *data = event->mimeData();
	QUrl url = data->urls().first();
	open(url.path());

	event->accept();
}


void MainWindow::closeEvent(QCloseEvent *event)
{
	_odm->clearAuthenticationOnExit(OnlineDataManager::OSF);

	if (_applicationExiting)
	{
		// sometimes on osx we get two events
		event->accept();
	}

	_applicationExiting = true;

	if (_package->isModified())
	{
		ui->backStage->close();
		event->ignore();
	}
	else
	{
		event->accept();
	}

	PreferencesDialog *rd = ui->tabBar->getPreferencesDialog();
	if (rd) rd->close();
}

bool MainWindow::filterShortCut()
{
	bool exclude = _excludeKey;
#ifdef __APPLE__
	if (exclude)
		qDebug() << "KEY EXCLUDED!";
	// This is a workaround for Qt Bug https://bugreports.qt.io/browse/QTBUG-67016
	// When we move to a Qt version (probably 5.11) where this bug is solved, we have to remove this workaround!
	_excludeKey = true;
	QTimer *timer = new QTimer(this);
	connect(timer, SIGNAL(timeout()), this, SLOT(updateExcludeKey()));
	timer->start(100);
#endif
	
	return exclude;
}

void MainWindow::saveKeysSelected()
{	
	if (filterShortCut())
		return;

	if (_package->isModified())
	{
		ui->backStage->save();
	}
}


void MainWindow::openKeysSelected()
{
	if (filterShortCut())
		return;
}


void MainWindow::refreshKeysSelected()
{
	if (filterShortCut())
		return;
	
	refreshAllAnalyses();
}

void MainWindow::zoomInKeysSelected()
{
	if (filterShortCut())
		return;
	
	_resultsJsInterface->zoomIn();
}

void MainWindow::zoomOutKeysSelected()
{
	if (filterShortCut())
		return;
	
	_resultsJsInterface->zoomOut();
}

void MainWindow::zoomEqualKeysSelected()
{
	if (filterShortCut())
		return;
	
	_resultsJsInterface->zoomReset();
}


void MainWindow::syncKeysSelected()
{
	if (filterShortCut())
		return;

	ui->backStage->sync();
}


void MainWindow::illegalOptionStateChanged(AnalysisForm * form)
{
	if (form->hasIllegalValue())
	{
		ui->optionsErrorLabel->setText(form->illegalValueMessage());
		ui->optionsErrorPanel->show();
	}
	else
	{
		ui->optionsErrorPanel->hide();
	}
}


void MainWindow::packageChanged(DataSetPackage *package)
{
	QString title = windowTitle();
	if (package->isModified())
	{
		setWindowTitle(title.append("*"));
	}
	else
	{
		title.chop(1);
		setWindowTitle(title);
	}
}


void MainWindow::refreshAnalysesUsingColumns(std::vector<std::string> &changedColumns,	 std::vector<std::string> &missingColumns,	 std::map<std::string, std::string> &changeNameColumns, bool rowCountChanged)
{
	std::vector<std::string> oldColumnNames;

	for (auto & keyval : changeNameColumns)
		oldColumnNames.push_back(keyval.first);

	sort(changedColumns.begin(), changedColumns.end());
	sort(missingColumns.begin(), missingColumns.end());
	sort(oldColumnNames.begin(), oldColumnNames.end());

	std::set<Analysis *> analysesToRefresh;

	for (Analysis* analysis : *_analyses)
	{
		if (analysis == NULL) continue;

		std::set<std::string> variables = analysis->usedVariables();

		if (!variables.empty())
		{
			std::vector<std::string> interChangecol, interChangename, interMissingcol;

			std::set_intersection(variables.begin(), variables.end(), changedColumns.begin(), changedColumns.end(), std::back_inserter(interChangecol));
			std::set_intersection(variables.begin(), variables.end(), oldColumnNames.begin(), oldColumnNames.end(), std::back_inserter(interChangename));
			std::set_intersection(variables.begin(), variables.end(), missingColumns.begin(), missingColumns.end(), std::back_inserter(interMissingcol));

			bool	aNameChanged	= interChangename.size() > 0,
					aColumnRemoved	= interMissingcol.size() > 0,
					aColumnChanged	= interChangecol.size() > 0;

			if(aNameChanged || aColumnRemoved)
				analysis->setRefreshBlocked(true);

			if (aColumnRemoved)
				for (std::string & varname : interMissingcol)
					analysis->removeUsedVariable(varname);

			if (aNameChanged)
				for (std::string & varname : interChangename)
					analysis->replaceVariableName(varname, changeNameColumns[varname]);

			if (aNameChanged || aColumnRemoved || aColumnChanged)
				analysesToRefresh.insert(analysis);
		}
	}

	for (Analysis *analysis : analysesToRefresh)
	{
		analysis->setRefreshBlocked(false);
		analysis->refresh();
	}

	_computedColumnsModel->packageSynchronized(changedColumns, missingColumns, changeNameColumns, rowCountChanged);
}

void MainWindow::dataSetChanged(DataSet * dataSet)
{
	_package->setDataSet(dataSet);
	setDataSetAndPackageInModels(_package);
}

void MainWindow::setPPIHandler(int ppi, bool refreshAllAnalyses)
{
	emit ppiChanged(ppi);

	if(refreshAllAnalyses)
		MainWindow::refreshAllAnalyses();
}

void MainWindow::setImageBackgroundHandler(QString value)
{
	emit imageBackgroundChanged(value);
	refreshAllAnalyses();
}

void MainWindow::setUIScaleHandler(float scale)
{
	ui->quickWidget_Data->rootContext()->setContextProperty("ppiScale",	scale);
}

void MainWindow::setDataSetAndPackageInModels(DataSetPackage *package)
{
	DataSet * dataSet = package == NULL ? NULL : package->dataSet();
	_tableModel->setDataSetPackage(package);
	_levelsTableModel->setDataSet(dataSet);
	_columnsModel->setDataSet(dataSet);
	_computedColumnsModel->setDataSetPackage(package);
}

void MainWindow::packageDataChanged(DataSetPackage *package,
									vector<string> &changedColumns,
									vector<string> &missingColumns,
									map<string, string> &changeNameColumns,
									bool rowCountChanged)
{
	setDataSetAndPackageInModels(package);

	_labelFilterGenerator->regenerateFilter();
	_filterModel->checkForSendFilter();
	refreshAnalysesUsingColumns(changedColumns, missingColumns, changeNameColumns, rowCountChanged);
}


void MainWindow::analysisResultsChangedHandler(Analysis *analysis)
{
	static bool showInstructions = true;

	if (showInstructions)
	{
		if (Settings::value(Settings::INSTRUCTIONS_SHOWN).toBool() == false)
		{
			Settings::setValue(Settings::INSTRUCTIONS_SHOWN, true);
			_resultsJsInterface->showInstruction();
		}

		showInstructions = false;
	}

	if (analysis->status() == Analysis::Running || (analysis->status() == Analysis::Inited && analysis->isAutorun()))
	{
		_runButton->setEnabled(true);
		_runButton->setText("Stop");
	}
	else
	{
		_runButton->setText("Run");
		if (analysis->status() == Analysis::InitedAndWaiting)
			_runButton->setEnabled(true);
		else
			_runButton->setEnabled(false);
	}

	_resultsJsInterface->analysisChanged(analysis);

	if (_package->isLoaded())
		_package->setModified(true);

	if(resultXmlCompare::compareResults::theOne()->testMode())
		analysesForComparingDoneAlready();
}

void MainWindow::analysisSaveImageHandler(int id, QString options)
{
	Analysis *analysis = _analyses->get(id);
	if (analysis == NULL)
		return;

	if (analysis->version() != AppInfo::version)
	{
		QMessageBox::StandardButton reply = QMessageBox::warning(this, "Version incompatibility", QString("This analysis was created in an older version of JASP, to save the image it must be refreshed first.\n\nRefresh the analysis?"), QMessageBox::Ok|QMessageBox::Cancel);

		if (reply == QMessageBox::Ok)
			analysis->refresh();
	}
	else
		_analysisSaveImageHandler(analysis, options);
}

void MainWindow::_analysisSaveImageHandler(Analysis* analysis, QString options)
{
	string utf8 = fq(options);
	Json::Value root;
	Json::Reader parser;
	parser.parse(utf8, root);

	QString caption = "Save JASP Image";
	QString filter = "Portable Network Graphics (*.png);;Portable Document Format (*.pdf);;Encapsulated PostScript (*.eps);;300 dpi Tagged Image File (*.tiff)";
	QString selectedFilter;

	QString finalPath = QFileDialog::getSaveFileName(this, caption, QString(), filter, &selectedFilter);
	if (!finalPath.isEmpty())
	{
		if (selectedFilter == "Encapsulated PostScript (*.eps)")
		{
			root["type"] = "eps";
			root["finalPath"] = finalPath.toStdString();
			analysis->saveImage(analysis, root);
		}
		else if (selectedFilter == "Portable Document Format (*.pdf)")
		{
			root["type"] = "pdf";
			root["finalPath"] = finalPath.toStdString();
			analysis->saveImage(analysis, root);
		}
		else if (selectedFilter == "300 dpi Tagged Image File (*.tiff)")
		{
			root["type"] = "tiff";
			root["finalPath"] = finalPath.toStdString();
			analysis->saveImage(analysis, root);
		}
		else
		{
			QString imagePath = QString::fromStdString(tempfiles_sessionDirName()) + "/" + root.get("name", Json::nullValue).asCString();
			if (QFile::exists(finalPath))
			{
				QFile::remove(finalPath);
			}
			QFile::copy(imagePath, finalPath);
		}
	}
}


void MainWindow::analysisImageSavedHandler(Analysis *analysis)
{
	Json::Value results = analysis->getImgResults();
	if (results.isNull())
		return;
	Json::Value inputOptions = results.get("inputOptions", Json::nullValue);

	QString imagePath = QString::fromStdString(tempfiles_sessionDirName()) + "/" + results.get("name", Json::nullValue).asCString();
	QString finalPath = QString::fromStdString(inputOptions.get("finalPath", Json::nullValue).asCString());

	if (!finalPath.isEmpty())
	{
		std::cout << "analysisImageSavedHandler, imagePath: " << imagePath.toStdString() << ", finalPath: " << finalPath.toStdString() << std::endl;
		std::cout.flush();
		if (QFile::exists(finalPath))
		{
			QFile::remove(finalPath);
		}
		QFile::copy(imagePath, finalPath);
	}
}

void MainWindow::analysisEditImageHandler(int id, QString options)
{

    Analysis *analysis = _analyses->get(id);
    if (analysis == NULL)
        return;

    string utf8 = fq(options);
    Json::Value root;
    Json::Reader parser;
    parser.parse(utf8, root);

    analysis->editImage(analysis, root);

    return;

}

AnalysisForm* MainWindow::loadForm(Analysis *analysis)
{
	if (_analysisFormsMap.count(analysis) == 0)
	{
		AnalysisForm * formCreated	= loadForm(analysis->name());
		_analysisFormsMap[analysis] = formCreated;

		//sizing of options widget and panel to fit buttons and conform to largest size for consistency
		for (QObject * child : formCreated->children())
		{
			QWidget* w = dynamic_cast<QWidget*>(child);
			if (w != NULL && w->objectName() == "topWidget")
			{
				w->setContentsMargins(0, 0, _buttonPanel->width(), 0);
				break;
			}
		}


		Options *options = analysis->options();
		DataSet *dataSet = _package->dataSet();
		formCreated->connectToAvailableVariablesModel(dataSet);
		formCreated->bindTo(options, dataSet);

		connect(formCreated, &AnalysisForm::illegalChanged, this, &MainWindow::illegalOptionStateChanged);
	}
	else
		_analysisFormsMap[analysis]->connectToAvailableVariablesModel(_package->dataSet());
	
	illegalOptionStateChanged(_analysisFormsMap[analysis]);
	_analysisFormsMap[analysis]->show();

	return _analysisFormsMap[analysis];
}

void MainWindow::updateShownVariablesModel()
{
	if(_currentOptionsWidget != NULL)
		_currentOptionsWidget->connectToAvailableVariablesModel(_package->dataSet());
}


AnalysisForm* MainWindow::loadForm(const string name)
{
	AnalysisForm *form = NULL;
std::cout << name << std::endl;

	QWidget *contentArea = ui->optionsContentArea;

	if		(name == "Ancova")										form = new AncovaForm(contentArea);
	else if (name == "AnovaOneWay")									form = new AnovaOneWayForm(contentArea);
	else if (name == "Correlation")									form = new CorrelationForm(contentArea);
	else if (name == "Descriptives")								form = new DescriptivesForm(contentArea);
	else if (name == "NetworkAnalysis")								form = new NetworkAnalysisForm(contentArea);
	else if (name == "MultinomialTest")								form = new MultinomialTestForm(contentArea);
	else if (name == "RegressionLinear")							form = new RegressionLinearForm(contentArea);
	else if (name == "AnovaMultivariate")							form = new AnovaMultivariateForm(contentArea);
	else if (name == "AncovaMultivariate")							form = new AncovaMultivariateForm(contentArea);
	else if (name == "CorrelationPartial")							form = new CorrelationPartialForm(contentArea);
	else if (name == "RegressionLogistic")							form = new RegressionLogisticForm(contentArea);
	else if (name == "RegressionLogLinear")							form = new RegressionLogLinearForm(contentArea);
	else if (name == "AnovaRepeatedMeasures")						form = new AnovaRepeatedMeasuresForm(contentArea);
	else if (name == "TTestBayesianOneSample")						form = new TTestBayesianOneSampleForm(contentArea);
	else if (name == "CorrelationBayesianPairs")					form = new CorrelationBayesianPairsForm(contentArea);
	else if (name == "ExploratoryFactorAnalysis")					form = new ExploratoryFactorAnalysisForm(contentArea);
	else if (name == "PrincipalComponentAnalysis")					form = new PrincipalComponentAnalysisForm(contentArea);
	else if (name == "RegressionLogLinearBayesian")					form = new RegressionLogLinearBayesianForm(contentArea);
	else if (name == "TTestBayesianIndependentSamples")				form = new TTestBayesianIndependentSamplesForm(contentArea);
	else if (name == "SummaryStatsRegressionLinearBayesian")		form = new SummaryStatsRegressionLinearBayesianForm(contentArea);
	else if (name == "SummaryStatsTTestBayesianIndependentSamples")	form = new SummaryStatsTTestBayesianIndependentSamplesForm(contentArea);
	else if (name == "SummaryStatsTTestBayesianPairedSamples")		form = new SummaryStatsTTestBayesianPairedSamplesForm(contentArea);
	else if (name == "SummaryStatsCorrelationBayesianPairs")		form = new SummaryStatsCorrelationBayesianPairsForm(contentArea);
	else if (name == "SummaryStatsTTestBayesianOneSample")			form = new SummaryStatsTTestBayesianOneSampleForm(contentArea);
	else if (name == "ReinforcementLearningR11tLearning")			form = new ReinforcementLearningR11tLearningForm(contentArea);
	else if (name == "SummaryStatsBinomialTestBayesian")			form = new SummaryStatsBinomialTestBayesianForm(contentArea);
	else if (name == "AnovaRepeatedMeasuresBayesian")				form = new AnovaRepeatedMeasuresBayesianForm(contentArea);
	else if (name == "TTestBayesianPairedSamples")					form = new TTestBayesianPairedSamplesForm(contentArea);
	else if (name == "ContingencyTablesBayesian")					form = new ContingencyTablesBayesianForm(contentArea);
	else if (name == "RegressionLinearBayesian")					form = new RegressionLinearBayesianForm(contentArea);
	else if (name == "TTestIndependentSamples")						form = new TTestIndependentSamplesForm(contentArea);
	else if (name == "ClassicalMetaAnalysis")						form = new ClassicalMetaAnalysisForm(contentArea);
	else if (name == "BinomialTestBayesian")						form = new BinomialTestBayesianForm(contentArea);
	else if (name == "CorrelationBayesian")							form = new CorrelationBayesianForm(contentArea);
	else if (name == "ReliabilityAnalysis")							form = new ReliabilityAnalysisForm(contentArea);
	else if (name == "TTestPairedSamples")							form = new TTestPairedSamplesForm(contentArea);
	else if (name == "ContingencyTables")							form = new ContingencyTablesForm(contentArea);
	else if (name == "TTestOneSample")								form = new TTestOneSampleForm(contentArea);
	else if (name == "AncovaBayesian")								form = new AncovaBayesianForm(contentArea);
	else if (name == "AnovaBayesian")								form = new AnovaBayesianForm(contentArea);
	else if (name == "BinomialTest")								form = new BinomialTestForm(contentArea);
	else if (name == "SEMSimple")									form = new SEMSimpleForm(contentArea);
	else if (name == "Anova")										form = new AnovaForm(contentArea);
    else if (name == "MLRegressionRandomForest")                    form = new MLRegressionRandomForestForm(contentArea);
    else if (name == "MLRegressionKNN")                             form = new MLRegressionKNNForm(contentArea);
    else if (name == "MLRegressionBoosting")                        form = new MLRegressionBoostingForm(contentArea);
    else if (name == "MLClassificationRandomForest")                form = new MLClassificationRandomForestForm(contentArea);
    else if (name == "MLClassificationKNN")                         form = new MLClassificationKNNForm(contentArea);
    else if (name == "MLClassificationBoosting")                    form = new MLClassificationBoostingForm(contentArea);
    else if (name == "MLClusteringKMeans")                          form = new MLClusteringKMeansForm(contentArea);
    else if (name == "MLClusteringRandomForest")                    form = new MLClusteringRandomForestForm(contentArea);
///// 4-analysis if-else ladder
	else
		qDebug() << "MainWindow::loadForm(); form not found : " << name.c_str();

	if(form != NULL)
	{
		connect(form,			&AnalysisForm::sendRScript, _engineSync,	&EngineSync::sendRCode);
		connect(_engineSync,	&EngineSync::rCodeReturned, form,			&AnalysisForm::runScriptRequestDone);
	}

	return form;
}


void MainWindow::showForm(Analysis *analysis)
{
	closeCurrentOptionsWidget();

	_currentOptionsWidget = loadForm(analysis);

	if (_currentOptionsWidget != NULL)
	{

		int requiredSize		= _currentOptionsWidget->sizeHint().width();
		int currentOptionSpace	= ui->panel_2_Options->minimumWidth() - _scrollbarWidth;

		if (requiredSize > currentOptionSpace)
		{
			ui->panel_2_Options->setMinimumWidth(requiredSize + _scrollbarWidth);
			_buttonPanel->move(ui->panel_2_Options->width() - _buttonPanel->width() - _scrollbarWidth, 0);
		}
		_currentOptionsWidget->setMinimumWidth(ui->panel_2_Options->minimumWidth() - _scrollbarWidth);

		_currentOptionsWidget->show();
		ui->optionsContentAreaLayout->addWidget(_currentOptionsWidget,0, 0, Qt::AlignRight | Qt::AlignTop);

		if (ui->panel_2_Options->isVisible() == false)
			showOptionsPanel();

		_okButton->setVisible(_currentAnalysis->useData());
		_runButton->setVisible(_currentAnalysis->isAutorun() == false);
		_runButton->setEnabled(_currentAnalysis->status() == Analysis::InitedAndWaiting);
		_buttonPanel->raise();
		_buttonPanel->show();

		QString helpPage = QString("analyses/") + tq(analysis->name()).toLower();
		requestHelpPage(helpPage);
	}
}


void MainWindow::closeCurrentOptionsWidget()
{
	if (_currentOptionsWidget != NULL)
	{
		_currentOptionsWidget->hide();
		_currentOptionsWidget = NULL;
	}
}


void MainWindow::analysisSelectedHandler(int id)
{
	_currentAnalysis = _analyses->get(id);

	if (_currentAnalysis != NULL)
	{
		showForm(_currentAnalysis);
		ui->tabBar->setCurrentTab(QString::fromStdString(_currentAnalysis->module()));
	}
}


void MainWindow::analysisUnselectedHandler()
{
	if (_currentAnalysis->useData())
		hideOptionsPanel();
}


void MainWindow::tabChanged(int index)
{
	if (index == 0)
	{
		ui->topLevelWidgets->setCurrentIndex(0);
	}
	else
	{
		ui->topLevelWidgets->setCurrentIndex(1); //Should be a reference to the mainPage

		QString currentActiveTab = ui->tabBar->getCurrentActiveTab();
		if (Module::isModuleName(currentActiveTab))
		{
			const Module& module = Module::getModule(currentActiveTab);
			ui->ribbon->setCurrentIndex(module.ribbonIndex());
		}
	}
}


void MainWindow::helpToggled(bool on)
{
	static int helpWidth = 0;

	if (on)
	{
		if (helpWidth < 400)
			helpWidth = 400;

		QList<int> sizes = ui->splitter->sizes();

		int resultsWidth = sizes.at(2) - ui->splitter->handleWidth() - 2 - helpWidth;

		sizes[2] = resultsWidth;
		sizes[3] = helpWidth;

		ui->panel_4_Help->show();
		ui->splitter->setSizes(sizes);
	}
	else
	{
		helpWidth = ui->panel_4_Help->width();
		ui->panel_4_Help->hide();
	}
}


void MainWindow::checkUsedModules()
{
	QStringList usedModules;
	for (Analyses::iterator itr = _analyses->begin(); itr != _analyses->end(); itr++)
	{
		Analysis *analysis = *itr;
		if (analysis != NULL && analysis->isVisible())
		{
			QString moduleName = QString::fromStdString(analysis->module());
			if (!usedModules.contains(moduleName))
				usedModules.append(moduleName);
		}
	}

	ui->tabBar->setModulePlusMenu(usedModules);
}


void MainWindow::dataSetIORequest(FileEvent *event)
{
	if (event->operation() == FileEvent::FileOpen)
	{
		if (_package->isLoaded())
		{
			//If this instance has a valid OSF connection save this setting for a new instance
			_odm->savePasswordFromAuthData(OnlineDataManager::OSF);

			// begin new instance
			QProcess::startDetached(QCoreApplication::applicationFilePath(), QStringList(event->path()));
		}
		else
		{
			connect(event, SIGNAL(completed(FileEvent*)), this, SLOT(dataSetIOCompleted(FileEvent*)));

			_loader.io(event, _package);
			showProgress();
		}

		ui->tabBar->setCurrentModuleActive();
	}
	else if (event->operation() == FileEvent::FileSave)
	{
		if (_analyses->count() > 0)
		{
			_package->setWaitingForReady();

			getAnalysesUserData();
			_resultsJsInterface->exportPreviewHTML();

			Json::Value analysesDataList = Json::arrayValue;
			for (Analyses::iterator itr = _analyses->begin(); itr != _analyses->end(); itr++)
			{
				Analysis *analysis = *itr;
				if (analysis != NULL && analysis->isVisible())
				{
					Json::Value analysisData = analysis->asJSON();
					analysisData["options"] = analysis->options()->asJSON();
					analysisData["userdata"] = analysis->userData();
					analysesDataList.append(analysisData);
				}
			}

			Json::Value analysesData = Json::objectValue;
			analysesData["analyses"] = analysesDataList;

			analysesData["meta"] = _resultsJsInterface->getResultsMeta();

			_package->setAnalysesData(analysesData);
		}

		connect(event, SIGNAL(completed(FileEvent*)), this, SLOT(dataSetIOCompleted(FileEvent*)));

		_loader.io(event, _package);
		showProgress();
	}
	else if (event->operation() == FileEvent::FileExportResults)
	{
		connect(event, SIGNAL(completed(FileEvent*)), this, SLOT(dataSetIOCompleted(FileEvent*)));

		_resultsJsInterface->exportHTML();

		_loader.io(event, _package);
		showProgress();
	}
	else if (event->operation() == FileEvent::FileExportData || event->operation() == FileEvent::FileGenerateData)
	{
		connect(event, SIGNAL(completed(FileEvent*)), this, SLOT(dataSetIOCompleted(FileEvent*)));
		_loader.io(event, _package);
		showProgress();
	}
	else if (event->operation() == FileEvent::FileSyncData)
	{
		if (_package->dataSet() == NULL)
			return;

		connect(event, SIGNAL(completed(FileEvent*)), this, SLOT(dataSetIOCompleted(FileEvent*)));
		_loader.io(event, _package);
		showProgress();
	}
	else if (event->operation() == FileEvent::FileClose)
	{
		if (_package->isModified())
		{
			QString title = windowTitle();
			title.chop(1);
			QMessageBox::StandardButton reply = QMessageBox::warning(this, "Save Workspace?", QString("Save changes to workspace \"") + title + QString("\" before closing?\n\nYour changes will be lost if you don't save them."), QMessageBox::Save|QMessageBox::Discard|QMessageBox::Cancel);

			if (reply == QMessageBox::Save)
			{
				FileEvent *saveEvent = ui->backStage->save();
				event->chain(saveEvent);
				connect(event, SIGNAL(completed(FileEvent*)), this, SLOT(dataSetIOCompleted(FileEvent*)));
				ui->panel_1_Data->hide();
			}
			else if (reply == QMessageBox::Cancel)
			{
				event->setComplete(false);
				dataSetIOCompleted(event);
			}
			else if (reply == QMessageBox::Discard)
			{
				event->setComplete(true);
				dataSetIOCompleted(event);
				ui->panel_1_Data->hide();
			}
		}
		else
		{
			event->setComplete();
			dataSetIOCompleted(event);
		}

		closeVariablesPage();
	}
}

void MainWindow::resizeVariablesWindowLabelColumn()
{
	QObject * levelsTableView = ui->quickWidget_Data->rootObject()->findChild<QObject*>("levelsTableView");
	QMetaObject::invokeMethod(levelsTableView, "resizeLabelColumn");
}

void MainWindow::closeVariablesPage()
{
	QObject * levelsTableView = ui->quickWidget_Data->rootObject()->findChild<QObject*>("levelsTableView");
	QMetaObject::invokeMethod(levelsTableView, "closeYourself");
}

void MainWindow::dataSetIOCompleted(FileEvent *event)
{
	this->analysisOKed();
	bool showAnalysis = false;
	hideProgress();

	if (event->operation() == FileEvent::FileOpen)
	{
		if (event->successful())
		{
			populateUIfromDataSet();
			QString name = QFileInfo(event->path()).baseName();
			setWindowTitle(name);
			_currentFilePath = event->path();

			if (event->type() == Utils::FileType::jasp && !_package->dataFilePath().empty() && !_package->dataFileReadOnly() && strncmp("http", _package->dataFilePath().c_str(), 4) != 0)
			{
				QString dataFilePath = QString::fromStdString(_package->dataFilePath());
				if (QFileInfo::exists(dataFilePath))
				{
					uint currentDataFileTimestamp = QFileInfo(dataFilePath).lastModified().toTime_t();
					if (currentDataFileTimestamp > _package->dataFileTimestamp())
						emit event->dataFileChanged(event->dataFilePath());
				}
				else
				{
					_package->setDataFilePath("");
					_package->setModified(true);
				}
			}

			if (resultXmlCompare::compareResults::theOne()->testMode())
				startComparingResults();
		}
		else
		{
			if (_package->dataSet() != NULL)
				_loader.free(_package->dataSet());
			_package->reset();
			setDataSetAndPackageInModels(NULL);

			QMessageBox::warning(this, "", "Unable to open file.\n\n" + event->message());

			if (_openedUsingArgs)
				close();
		}
	}
	else if (event->operation() == FileEvent::FileSave)
	{
		if (event->successful())
		{
			QString name = QFileInfo(event->path()).baseName();

			_package->setModified(false);
			setWindowTitle(name);
			showAnalysis = true;
		}
		else
		{
			QMessageBox::warning(this, "", "Unable to save file.\n\n" + event->message());
		}
	}
	else if (event->operation() == FileEvent::FileSyncData)
	{
		_package->setModified(true);
		showAnalysis = true;
	}
	else if (event->operation() == FileEvent::FileGenerateData || event->operation() == FileEvent::FileExportResults)
	{
		showAnalysis = true;
	}
	else if (event->operation() == FileEvent::FileClose)
	{
		if (event->successful())
		{
			closeCurrentOptionsWidget();
			for (auto &keyvalue : _analysisFormsMap)
			{
				AnalysisForm* form = keyvalue.second;
				delete form;
			}
			_analysisFormsMap.clear();
			_analyses->clear();
			hideOptionsPanel();
			setDataSetAndPackageInModels(NULL);
			_loader.free(_package->dataSet());
			_package->reset();
			_filterModel->setDataSetPackage(NULL);
			updateMenuEnabledDisabledStatus();
			ui->webViewResults->reload();
			setWindowTitle("JASP");


			if (_applicationExiting)
				QApplication::exit();
			else
				ui->panel_1_Data->hide();
		}
		else
		{
			_applicationExiting = false;
		}
	}

	if (showAnalysis)
	{
		ui->tabBar->setCurrentModuleActive();
	}
}


void MainWindow::populateUIfromDataSet()
{
	setDataSetAndPackageInModels(_package);

	if(_package->dataSet()->rowCount() == 0)
		ui->panel_1_Data->hide(); //for summary stats etc we dont want to see an empty data panel
	else
	{
		_filterModel->setDataSetPackage(_package);
		_filterModel->init();
	}
	
	hideProgress();

	bool errorFound = false;
	stringstream errorMsg;

	if (_package->hasAnalyses())
	{
		int corruptAnalyses = 0;

		stringstream corruptionStrings;

		Json::Value analysesData = _package->analysesData();
		if (analysesData.isNull())
		{
			errorFound = true;
			errorMsg << "An error has been detected and analyses could not be loaded.";
		}
		else
		{
			Json::Value analysesDataList = analysesData;
			if (!analysesData.isArray()) {
				analysesDataList = analysesData.get("analyses", Json::arrayValue);
				Json::Value meta = analysesData.get("meta", Json::nullValue);
				if ( ! meta.isNull())
				{
					QString results = tq(analysesData["meta"].toStyledString());
					_resultsJsInterface->setResultsMeta(results);
				}
			}

			for (Json::ValueIterator iter = analysesDataList.begin(); iter != analysesDataList.end(); iter++)
			{
				try
				{
					Json::Value &analysisData = *iter;

					QString name = QString::fromStdString(analysisData["name"].asString());
					QString module = QString::fromStdString(analysisData["module"].asString());
					if (module.isEmpty())
						module = "Common";
					int id = analysisData["id"].asInt();

					Json::Value &optionsJson	= analysisData["options"];
					Json::Value &resultsJson	= analysisData["results"];
					Json::Value &userDataJson	= analysisData["userdata"];
					Json::Value &versionJson	= analysisData["version"];

					Version version				= versionJson.isNull() ? AppInfo::version : Version(versionJson.asString());

					Analysis::Status status		= Analysis::parseStatus(analysisData["status"].asString());

					Analysis *analysis			= _analyses->create(module, name, id, version, &optionsJson, status);

					analysis->setUserData(userDataJson);
					analysis->setResults(resultsJson);
				}
				catch (runtime_error e)
				{
					errorFound = true;
					corruptionStrings << "\n" << (++corruptAnalyses) << ": " << e.what();
				}
				catch (exception e)
				{
					errorFound = true;
					corruptionStrings << "\n" << (++corruptAnalyses) << ": " << e.what();
				}
			}
		}

		if (corruptAnalyses == 1)
			errorMsg << "An error was detected in an analyses. This analyses has been removed for the following reason:\n" << corruptionStrings.str();
		else if (corruptAnalyses > 1)
			errorMsg << "Errors were detected in " << corruptAnalyses << " analyses. These analyses have been removed for the following reasons:\n" << corruptionStrings.str();
	}

	if (_package->warningMessage() != "")	QMessageBox::warning(this, "", tq(_package->warningMessage()));
	else if (errorFound)					QMessageBox::warning(this, "", tq(errorMsg.str()));

	matchComputedColumnsToAnalyses();

	_package->setLoaded();
	updateMenuEnabledDisabledStatus();
	checkUsedModules();
}

void MainWindow::matchComputedColumnsToAnalyses()
{
	for(ComputedColumn * col : *_package->computedColumnsPointer())
		if(col->analysisId() != -1)
			col->setAnalysis(_analyses->get(col->analysisId()));
}


void MainWindow::updateMenuEnabledDisabledStatus()
{
	bool loaded = _package->isLoaded();

	ui->ribbonAnalysis->setDataSetLoaded(loaded);
	ui->ribbonSEM->setDataSetLoaded(loaded);
	ui->ribbonReinforcementLearning->setDataSetLoaded(loaded);
	ui->ribbonMetaAnalysis->setDataSetLoaded(loaded);
	ui->ribbonNetworkAnalysis->setDataSetLoaded(loaded);
	ui->ribbonMachineLearning->setDataSetLoaded(loaded);
///// 5-ribbon updateMenuEnabledDisabledStatus
}

void MainWindow::resultsPageLoaded(bool success, int ppi)
{
	if (success)
	{
// #ifdef __WIN32__
// 		const int verticalDpi = QApplication::desktop()->screen()->logicalDpiY();
// 		qreal zoom = ((qreal)(verticalDpi) / (qreal)ppi);
// 		ui->webViewResults->setZoomFactor(zoom);
// 		ui->webViewHelp->setZoomFactor(zoom);
// 		ppi = verticalDpi;
// 		_resultsJsInterface->setZoom(zoom);
//
// 		this->resize(this->width() + (ui->webViewResults->width() * (zoom - 1)), this->height() + (ui->webViewResults->height() * (zoom - 1)));
// #endif

		if (_openOnLoadFilename != "")
		{
			ui->backStage->open(_openOnLoadFilename);
			_openOnLoadFilename = "";
		}

		_resultsViewLoaded = true;
	}

	if (_engineSync->engineStarted() == false)
		_engineSync->start();

	PreferencesDialog *rd = ui->tabBar->getPreferencesDialog();
	rd->setDefaultPPI(ppi);

	bool useDefaultPPI = Settings::value(Settings::PPI_USE_DEFAULT).toBool();
	if (!useDefaultPPI)
	{
		int customPPI = Settings::value(Settings::PPI_CUSTOM_VALUE).toInt();
		ppi = customPPI;
	}

	setPPIHandler(ppi, false);
}


void MainWindow::fatalError()
{
	static bool exiting = false;

	if (exiting == false)
	{
		exiting = true;

		QMessageBox::warning(this, "Error", "JASP has experienced an unexpected internal error.\n\n\"" + _fatalError + "\"\n\nIf you could report your experiences to the JASP team that would be appreciated.\n\nJASP cannot continue and will now close.\n\n");
		QApplication::exit(1);
	}
}


void MainWindow::helpFirstLoaded(bool ok)
{
	if (ok)
		requestHelpPage("index");
}

void MainWindow::showHelpFromQML(QString pageName)
{
	if(_lastRequestedHelpPage == pageName && ui->panel_4_Help->isVisible())
	{
		helpToggled(false);
	}
	else
	{
		if(!ui->panel_4_Help->isVisible())
			helpToggled(true);

		requestHelpPage(pageName);
	}
}

void MainWindow::requestHelpPage(const QString &pageName)
{
	QFile fileMD(AppDirs::help() + "/" + pageName + ".md"), fileHTML(AppDirs::help() + "/" + pageName + ".html");

	QString content, renderFunc = "window.render";


	if (fileHTML.exists())
	{
		fileHTML.open(QFile::ReadOnly);
		content = QString::fromUtf8(fileHTML.readAll());
		fileHTML.close();

		renderFunc = "window.renderHtml";

	}
	else if (fileMD.exists())
	{
		fileMD.open(QFile::ReadOnly);
		content = QString::fromUtf8(fileMD.readAll());
		fileMD.close();
	}
	else
		content = "Coming Soon!\n========\n\nThere is currently no help available for this analysis.\n\nAdditional documentation will be available in future releases of JASP.";

	content.replace("\"", "\\\"");
	content.replace("\r\n", "\\n");
	content.replace("\r", "\\n");
	content.replace("\n", "\\n");

	ui->webViewHelp->page()->runJavaScript(renderFunc + "(\"" + content + "\")");

	_lastRequestedHelpPage = pageName;
}


void MainWindow::emptyValuesChangedHandler()
{
	if (_package->isLoaded())
	{
		vector<string> colChanged;
		vector<string> missingColumns;
		map<string, string> changeNameColumns;

		try
		{
			colChanged = _package->dataSet()->resetEmptyValues(_package->emptyValuesMap());
		}
		catch (boost::interprocess::bad_alloc &e)
		{
			try {

				_package->setDataSet(SharedMemory::enlargeDataSet(_package->dataSet()));
				colChanged = _package->dataSet()->resetEmptyValues(_package->emptyValuesMap());
			}
			catch (exception &e)
			{
				throw runtime_error("Out of memory: this data set is too large for your computer's available memory");
			}
		}
		catch (exception e)	{	cout << "MainWindow::emptyValuesChangedHandler n " << e.what() << std::endl; 	}
		catch (...)			{	cout << "MainWindow::emptyValuesChangedHandler something when wrong...\n" << std::endl; }

		_package->setModified(true);
		packageDataChanged(_package, colChanged, missingColumns, changeNameColumns, false);
	}
}

void MainWindow::itemSelected(const QString &item)
{
	try
	{
		QString currentActiveTab = ui->tabBar->getCurrentActiveTab();
		const Module& module = Module::getModule(currentActiveTab);

		_currentAnalysis = _analyses->create(module.name(), item);

		showForm(_currentAnalysis);
		_resultsJsInterface->showAnalysis(_currentAnalysis->id());

		QString info("%1,%2");
		info = info.arg(tq(_currentAnalysis->name()));
		info = info.arg(_currentAnalysis->id());

		checkUsedModules();
	}
	catch (runtime_error& e)
	{
		_fatalError = tq(e.what());
		fatalError();
	}
}


void MainWindow::saveTextToFileHandler(const QString &filename, const QString &data)
{
	if (filename == "%PREVIEW%" || filename == "%EXPORT%")
	{
		_package->setAnalysesHTML(fq(data));
		_package->setAnalysesHTMLReady();

		finishComparingResults();
	}
	else
	{
		QFile file(filename);
		file.open(QIODevice::WriteOnly | QIODevice::Truncate);
		QTextStream stream(&file);
		stream.setCodec("UTF-8");

		stream << data;
		stream.flush();
		file.close();
	}
}

void MainWindow::adjustOptionsPanelWidth()
{
	if (ui->panel_2_Options->width() == ui->panel_2_Options->maximumWidth() && ui->panel_1_Data->isHidden())
	{
		showDataPanel();
	}
	else if (ui->panel_1_Data->width() == ui->panel_1_Data->minimumWidth() && ui->panel_1_Data->isVisible() && ui->panel_2_Options->isVisible())
	{
		hideDataPanel();
	}

	_buttonPanel->move(ui->panel_2_Options->width() - _buttonPanel->width() - _scrollbarWidth, 0);
}


void MainWindow::splitterMovedHandler(int, int)
{
	adjustOptionsPanelWidth();
	_tableViewWidthBeforeOptionsMadeVisible = -1;
}


void MainWindow::hideOptionsPanel()
{
	int newTableWidth = 0;

	QList<int> sizes = ui->splitter->sizes();

	if (_tableViewWidthBeforeOptionsMadeVisible > 0)
	{
		newTableWidth = _tableViewWidthBeforeOptionsMadeVisible;
	}
	else
	{
		newTableWidth += sizes.at(0);

		if (ui->panel_1_Data->isVisible())
			newTableWidth += ui->splitter->handleWidth() + 2;

		newTableWidth += sizes.at(1);
	}

	sizes[0] = newTableWidth;
	sizes[1] = 0;

	ui->panel_2_Options->hide();
	if(_package != NULL && _package->dataSet() != NULL && _package->dataSet()->rowCount() > 0)		ui->panel_1_Data->show();
	else																							ui->panel_1_Data->hide();
	ui->splitter->setSizes(sizes);
}


void MainWindow::showOptionsPanel()
{
	QList<int> sizes = ui->splitter->sizes();

	int tableWidth = sizes.at(0);
	int newTableWidth = tableWidth;
	newTableWidth -= ui->panel_2_Options->minimumWidth();
	newTableWidth -= ui->splitter->handleWidth();

	ui->panel_2_Options->show();

	if (newTableWidth < ui->panel_1_Data->minimumWidth())
	{
		int midPanelWidth = tableWidth;
		if (midPanelWidth < ui->panel_2_Options->minimumWidth())
		{
			_tableViewWidthBeforeOptionsMadeVisible = midPanelWidth;
			midPanelWidth = ui->panel_2_Options->minimumWidth();
		}

		int w = 0;
		w += ui->panel_2_Options->minimumWidth();
		w += ui->panel_1_Data->minimumWidth();
		w += ui->splitter->handleWidth();

		ui->panel_2_Options->setMaximumWidth(w+8);
		ui->panel_1_Data->hide();

		sizes[0] = 0;
		sizes[1] = midPanelWidth;

		ui->splitter->setSizes(sizes);
	}
	else
	{
		ui->panel_2_Options->setMaximumWidth(ui->panel_2_Options->minimumWidth());

		sizes[0] = newTableWidth - 2;
		sizes[1] = ui->panel_2_Options->minimumWidth();

		ui->splitter->setSizes(sizes);
	}

	_buttonPanel->move(ui->panel_2_Options->width() - _buttonPanel->width() - _scrollbarWidth, 0);
}


void MainWindow::showDataPanel()
{
	QList<int> sizes = ui->splitter->sizes();

	sizes[0] = ui->panel_1_Data->minimumWidth()+8;
	sizes[1] = ui->panel_2_Options->minimumWidth();

	ui->splitter->setSizes(sizes);

	ui->panel_2_Options->setMaximumWidth(ui->panel_2_Options->minimumWidth());
	ui->panel_1_Data->show();
}


void MainWindow::hideDataPanel()
{
	QList<int> sizes = ui->splitter->sizes();

	int w = 0;
	w += ui->panel_2_Options->minimumWidth();
	w += ui->panel_1_Data->minimumWidth();
	w += ui->splitter->handleWidth();

	ui->panel_2_Options->setMaximumWidth(w+8);
	ui->panel_1_Data->hide();

	sizes[0] = 0;
	sizes[1] = w;

	ui->splitter->setSizes(sizes);
}


void MainWindow::analysisOKed()
{
	if (_currentOptionsWidget != NULL)
		closeCurrentOptionsWidget();

	_resultsJsInterface->unselect();

	hideOptionsPanel();
}


void MainWindow::analysisRunned()
{
	if (_currentAnalysis == NULL)
		return;

	if (_currentAnalysis->status() == Analysis::Running)
		_currentAnalysis->abort();
	else if (_currentAnalysis->status() == Analysis::InitedAndWaiting)
		_currentAnalysis->scheduleRun();
}


void MainWindow::removeAnalysis(Analysis *analysis)
{
	bool selected = false;
	analysis->abort();

	if (_currentOptionsWidget != NULL && analysis == _currentAnalysis)
	{
		selected = true;
		closeCurrentOptionsWidget();
	}
	
	delete _analysisFormsMap[analysis];
	_analysisFormsMap.erase(analysis);

	analysis->setVisible(false);

	if (_package->isLoaded())
		_package->setModified(true);


	_resultsJsInterface->removeAnalysis(analysis);

	if (selected)
		hideOptionsPanel();
	checkUsedModules();
}


void MainWindow::removeAllAnalyses()
{
	QMessageBox::StandardButton reply;
	reply = QMessageBox::question(this, "Remove All Analyses", " Do you really want to remove all analyses ? ",
								  QMessageBox::Yes|QMessageBox::No,QMessageBox::Yes);
	if (reply == QMessageBox::Yes)
	{
		for (Analyses::iterator itr = _analyses->begin(); itr != _analyses->end(); itr++)
		{
			Analysis *analysis = *itr;
			if (analysis == NULL) continue;
			removeAnalysis(analysis);
		}
	}
}


void MainWindow::refreshAllAnalyses()
{
	for (Analyses::iterator it = _analyses->begin(); it != _analyses->end(); ++it)
	{
		Analysis *analysis = *it;
		if (analysis == NULL) continue;
		analysis->refresh();
	}
}


void MainWindow::refreshAnalysesUsingColumn(QString col)
{
	std::vector<std::string> changedColumns, missingColumns;
	std::map<std::string, std::string> changeNameColumns;
	changedColumns.push_back(col.toStdString());
	refreshAnalysesUsingColumns(changedColumns, missingColumns, changeNameColumns, false);

	//_package->setModified(false); //Why would we do this?
}

void MainWindow::removeAnalysisRequestHandler(int id)
{
	Analysis *analysis = _analyses->get(id);
	removeAnalysis(analysis);
}

void MainWindow::getAnalysesUserData()
{
	QVariant userData = _resultsJsInterface->getAllUserData();

	Json::Value data;
	Json::Reader parser;
	parser.parse(fq(userData.toString()), data);

	for (Json::Value::iterator iter = data.begin(); iter != data.end(); iter++)
	{
		Json::Value &userDataObj = *iter;

		Analysis *analysis = _analyses->get(userDataObj["id"].asInt());

		Json::Value &analysisUserData = userDataObj["userdata"];

		analysis->setUserData(analysisUserData);
	}
}

void MainWindow::setPackageModified()
{
	_package->setModified(true);
}



void MainWindow::analysisChangedDownstreamHandler(int id, QString options)
{
	Analysis *analysis = _analyses->get(id);
	if (analysis == NULL)
		return;

	string utf8 = fq(options);

	Json::Value root;

	Json::Reader parser;
	parser.parse(utf8, root);

	analysis->options()->set(root);
}

void MainWindow::startDataEditorHandler()
{
	QString path = QString::fromStdString(_package->dataFilePath());
	if (path.isEmpty() || path.startsWith("http") || !QFileInfo::exists(path) || Utils::getFileSize(path.toStdString()) == 0 || _package->dataFileReadOnly())
	{
		QString									message = "JASP was started without associated data file (csv, sav or ods file). But to edit the data, JASP starts a spreadsheet editor based on this file and synchronize the data when the file is saved. Does this data file exist already, or do you want to generate it?";
		if (path.startsWith("http"))			message = "JASP was started with an online data file (csv, sav or ods file). But to edit the data, JASP needs this file on your computer. Does this data file also exist on your computer, or do you want to generate it?";
		else if (_package->dataFileReadOnly())	message = "JASP was started with a read-only data file (probably from the examples). But to edit the data, JASP needs to write to the data file. Does the same file also exist on your computer, or do you want to generate it?";

		QMessageBox msgBox(QMessageBox::Question, QString("Start Spreadsheet Editor"), message, QMessageBox::Yes | QMessageBox::No | QMessageBox::Cancel);
		msgBox.setButtonText(QMessageBox::Yes, QString("Generate Data File"));
		msgBox.setButtonText(QMessageBox::No, QString("Find Data File"));
		int reply = msgBox.exec();
		if (reply == QMessageBox::Cancel)
			return;

		FileEvent *event = NULL;
		if (reply == QMessageBox::Yes)
		{
			QString caption = "Generate Data File as CSV";
			QString filter = "CSV Files (*.csv)";
			QString name = windowTitle();
			if (name.endsWith("*"))
			{
				name.truncate(name.length() - 1);
				name = name.replace('#', '_');
			}
			if (!_currentFilePath.isEmpty())
			{
				QFileInfo file(_currentFilePath);
				name = file.absolutePath() + QDir::separator() + file.baseName().replace('#', '_') + ".csv";
			}

			path = QFileDialog::getSaveFileName(this, caption, name, filter);
			if (path == "")
				return;

			if (!path.endsWith(".csv", Qt::CaseInsensitive))
				path.append(".csv");

			event = new FileEvent(this, FileEvent::FileGenerateData);
		}
		else
		{
			QString caption = "Find Data File";
			QString filter = "Data File (*.csv *.txt *.sav *.ods)";

			path = QFileDialog::getOpenFileName(this, caption, "", filter);
			if (path == "")
				return;

			event = new FileEvent(this, FileEvent::FileSyncData);
		}

		connect(event, SIGNAL(completed(FileEvent*)), this, SLOT(startDataEditorEventCompleted(FileEvent*)));
		connect(event, SIGNAL(completed(FileEvent*)), ui->backStage, SLOT(setSyncFile(FileEvent*)));
		event->setPath(path);
		_loader.io(event, _package);
		showProgress();
	}
	else
		startDataEditor(path);
}


void MainWindow::startDataEditorEventCompleted(FileEvent* event)
{
	hideProgress();

	if (event->successful())
	{
		_package->setDataFilePath(event->path().toStdString());
		_package->setDataFileReadOnly(false);
		_package->setModified(true);
		startDataEditor(event->path());
	}
}


void MainWindow::startDataEditor(QString path)
{
	QFileInfo fileInfo(path);

	int useDefaultSpreadsheetEditor = Settings::value(Settings::USE_DEFAULT_SPREADSHEET_EDITOR).toInt();
	QString appname = Settings::value(Settings::SPREADSHEET_EDITOR_NAME).toString();

	if (QString::compare(fileInfo.suffix(), "sav", Qt::CaseInsensitive) == 0)
	{
		if (useDefaultSpreadsheetEditor == 0 && !appname.contains("SPSS", Qt::CaseInsensitive))
			useDefaultSpreadsheetEditor = 1;
	}

	if (appname.isEmpty())
		useDefaultSpreadsheetEditor = 1;

	QString startProcess;
	if (useDefaultSpreadsheetEditor == 0)
	{
#ifdef __APPLE__
		appname = appname.mid(appname.lastIndexOf('/') + 1);
		startProcess = "open -a \"" + appname + "\" \"" + path + "\"";
#else
		startProcess = "\"" + appname + "\" \"" + path + "\"";
#endif
		if (!QProcess::startDetached(startProcess))
		{
			QMessageBox::warning(this,QString("Start Editor"), QString("Unable to start the editor : ") + appname + QString(". Please check your editor settings in the preference menu."), QMessageBox::Ok);
		}
	}
	else
	{
		if (!QDesktopServices::openUrl(QUrl("file:///" + path, QUrl::TolerantMode)))
		{
			QMessageBox::warning(this, QString("Start Spreadsheet Editor"), QString("No default spreadsheet editor for file ") + fileInfo.completeBaseName() + QString(". Use Preferences to set the right editor."), QMessageBox::Cancel);
		}
	}
}

void MainWindow::showProgress()
{
	ui->panel_1_Data->show();
	QMetaObject::invokeMethod(qmlProgressBar, "show");
}

void MainWindow::hideProgress()
{
	QMetaObject::invokeMethod(qmlProgressBar, "hide");
}

void MainWindow::setProgressStatus(QString status, int progress)
{
	QMetaObject::invokeMethod(qmlProgressBar, "setStatus", Q_ARG(QVariant, QVariant(status)), Q_ARG(QVariant, QVariant(progress)));
}


void MainWindow::updateExcludeKey()
{
	_excludeKey = false;
}


void MainWindow::testLoadedJaspFile(int timeOut)
{
	std::cout << "Enabling testmode for JASP with a timeout of " << timeOut << " minutes!" << std::endl;
	resultXmlCompare::compareResults::theOne()->enableTestMode();

	QTimer::singleShot(60000 * timeOut, this, &MainWindow::unitTestTimeOut);
}

void MainWindow::unitTestTimeOut()
{
	std::cerr << "Time out for unit test!" << std::endl;
	_application->exit(2);
}

void MainWindow::startComparingResults()
{
	if (resultXmlCompare::compareResults::theOne()->testMode())
	{
		refreshAllAnalyses();
		resultXmlCompare::compareResults::theOne()->setRefreshCalled();
	}
}



void MainWindow::analysesForComparingDoneAlready()
{
	if(resultXmlCompare::compareResults::theOne()->testMode() && resultXmlCompare::compareResults::theOne()->refreshed())
	{
		bool allCompleted = true;

		for(Analysis * analysis : *_analyses)
			if(analysis != NULL && !analysis->isFinished())
				allCompleted = false;

		if(allCompleted)
		{
			_resultsJsInterface->exportPreviewHTML();
			resultXmlCompare::compareResults::theOne()->setExportCalled();
		}
	}
}

void MainWindow::finishComparingResults()
{
	if(resultXmlCompare::compareResults::theOne()->testMode() && resultXmlCompare::compareResults::theOne()->exportCalled())
	{
		std::string resultHtml = _package->analysesHTML();
		resultXmlCompare::compareResults::theOne()->setRefreshResult(QString::fromStdString(resultHtml));

		bool success = resultXmlCompare::compareResults::theOne()->compare();

		_application->exit(success ? 0 : 1);
	}
}

void MainWindow::pauseEngines()
{
	_engineSync->pause();
}

void MainWindow::resumeEngines()
{
	_engineSync->resume();
}
