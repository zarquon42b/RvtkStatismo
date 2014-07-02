#ifndef _STATISMO_REDUCED_MODEL_H__
#define _STATISMO_REDUCED_MODEL_H__
#include "VTKTypes.h"
#include "ReducedVarianceModelBuilder.h"
#include <RcppEigen.h>
#include "pPCA2statismo.h"

RcppExport SEXP ReducedModel(SEXP pPCA_,SEXP npc_,SEXP exVar_, SEXP scores_);
#endif
