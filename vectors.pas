UNIT vectors;

{$mode objfpc}{$H+}

INTERFACE
USES GL;
TYPE
  Tfloat  =double;
  TVector3=array[0..2] of Tfloat;
  TVector3x5=array[0..4] of TVector3;
  PVector3=^TVector3;
  TIntVec3=array[0..2] of longint;
  TMatrix3x3=array[0..2] of TVector3;

FUNCTION vectorOf(CONST x,y,z:Tfloat):TVector3;
FUNCTION randomInSphere:TVector3;
FUNCTION randomOnSphere:TVector3;
OPERATOR * (CONST x:TVector3; CONST y:Tfloat):TVector3;
OPERATOR * (CONST x,y:TVector3):Tfloat;
OPERATOR +(CONST x,y:TVector3):TVector3;
OPERATOR -(CONST x,y:TVector3):TVector3;
OPERATOR -(CONST x,y:TIntVec3):TIntVec3;
FUNCTION sumOfSquares(CONST x:TVector3):Tfloat;
FUNCTION sqrEuklideanNorm(CONST x:TVector3):Tfloat;
FUNCTION euklideanNorm(CONST x:TVector3):Tfloat;
FUNCTION euklideanNorm(CONST x:TIntVec3):Tfloat;
FUNCTION cross(CONST x,y:TVector3):TVector3;
FUNCTION hsvColor(h,s,v:Tfloat):TVector3;

FUNCTION roundVector(CONST v:TVector3):TIntVec3;
OPERATOR *(CONST x:TIntVec3; CONST y:Tfloat):TVector3;
OPERATOR =(CONST x,y:TIntVec3):boolean;

FUNCTION randomOrthonormalBasis:TMatrix3x3;
FUNCTION orthonormalBasisOf(CONST v1,v2:TVector3):TMatrix3x3;
FUNCTION invert(CONST m:TMatrix3x3):TMatrix3x3;
FUNCTION transpose(CONST m:TMatrix3x3):TMatrix3x3;
OPERATOR * (CONST A:TMatrix3x3; CONST v:TVector3):TVector3;

OPERATOR * (CONST x:TVector3x5; CONST y:Tfloat):TVector3x5;
OPERATOR + (CONST x,y:TVector3x5):TVector3x5;
FUNCTION maxNormDiff(CONST x,y:TVector3x5):Tfloat;

CONST ZERO_VECTOR:TVector3=(0,0,0);
IMPLEMENTATION
USES math;
FUNCTION vectorOf(CONST x,y,z:Tfloat):TVector3;
  begin
    result[0]:=x;
    result[1]:=y;
    result[2]:=z;
  end;

FUNCTION randomInSphere:TVector3;
  begin
    repeat
      result[0]:=2*random-1;
      result[1]:=2*random-1;
      result[2]:=2*random-1;
    until sumOfSquares(result)<1;
  end;

FUNCTION randomOnSphere:TVector3;
  VAR n:Tfloat;
  begin
    repeat
      result[0]:=2*random-1;
      result[1]:=2*random-1;
      result[2]:=2*random-1;
      n:=euklideanNorm(result);
    until (n<1) and (n>1E-2);
    result*=1/n;
  end;

OPERATOR * (CONST x:TVector3; CONST y:Tfloat):TVector3;
  begin
    result[0]:=x[0]*y;
    result[1]:=x[1]*y;
    result[2]:=x[2]*y;
  end;

OPERATOR * (CONST x,y:TVector3):Tfloat;
  begin
    result:=x[0]*y[0]+x[1]*y[1]+x[2]*y[2];
  end;

OPERATOR +(CONST x,y:TVector3):TVector3;
  begin
    result[0]:=x[0]+y[0];
    result[1]:=x[1]+y[1];
    result[2]:=x[2]+y[2];
  end;

OPERATOR -(CONST x,y:TVector3):TVector3;
  begin
    result[0]:=x[0]-y[0];
    result[1]:=x[1]-y[1];
    result[2]:=x[2]-y[2];
  end;

OPERATOR -(CONST x,y:TIntVec3):TIntVec3;
  begin
    result[0]:=x[0]-y[0];
    result[1]:=x[1]-y[1];
    result[2]:=x[2]-y[2];
  end;

FUNCTION sumOfSquares(CONST x: TVector3): Tfloat;
  begin
    result:=x[0]*x[0]+x[1]*x[1]+x[2]*x[2];
  end;

FUNCTION sqrEuklideanNorm(CONST x:TVector3):Tfloat;
  begin
    result:=x[0]*x[0]+x[1]*x[1]+x[2]*x[2];
  end;

FUNCTION euklideanNorm(CONST x:TVector3):Tfloat;
  begin
    result:=sqrt(x[0]*x[0]+x[1]*x[1]+x[2]*x[2]);
  end;

FUNCTION euklideanNorm(CONST x:TIntVec3):Tfloat;
  begin
    result:=sqrt(x[0]*x[0]+x[1]*x[1]+x[2]*x[2]);
  end;

FUNCTION cross(CONST x,y:TVector3):TVector3;
  begin
    result[0]:=x[1]*y[2]-x[2]*y[1];
    result[1]:=x[2]*y[0]-x[0]*y[2];
    result[2]:=x[0]*y[1]-x[1]*y[0];
  end;

FUNCTION hsvColor(h,s,v:Tfloat):TVector3;
  VAR hi:byte;
      p,q,t:Tfloat;
  begin
    initialize(result);
    result[0]:=0;
    result[1]:=0;
    result[2]:=0;
    if isInfinite(h) or isNan(h) then exit(result);
    if h>1 then h:=frac(h)
    else if h<0 then h:=1+frac(h);

    while h<0 do h:=h+1;
    while h>1 do h:=h-1;

    hi:=trunc(h*6); h:=h*6-hi;
    p:=v*(1-s      );
    q:=v*(1-s*   h );
    t:=v*(1-s*(1-h));
    case hi of
      1  : result:=vectorOf(q,v,p);
      2  : result:=vectorOf(p,v,t);
      3  : result:=vectorOf(p,q,v);
      4  : result:=vectorOf(t,p,v);
      5  : result:=vectorOf(v,p,q);
      else result:=vectorOf(v,t,p);
    end;
  end;

FUNCTION roundVector(CONST v: TVector3): TIntVec3;
  begin
    result[0]:=round(v[0]);
    result[1]:=round(v[1]);
    result[2]:=round(v[2]);
  end;

OPERATOR*(CONST x: TIntVec3; CONST y: Tfloat): TVector3;
  begin
    result[0]:=x[0]*y;
    result[1]:=x[1]*y;
    result[2]:=x[2]*y;
  end;

OPERATOR=(CONST x, y: TIntVec3): boolean;
  begin
    result:=(x[0]=y[0]) and (x[1]=y[1]) and (x[2]=y[2]);
  end;

FUNCTION randomOrthonormalBasis: TMatrix3x3;
  begin
    //First vector is random
    result[0]:=randomOnSphere;
    //Second vector is chosen to be orthogonal to the first one
    result[1]:=cross(result[0],randomOnSphere);
    result[1]*=1/euklideanNorm(result[1]);
    //Third vector is cross product of the previous two
    result[2]:=cross(result[0],result[1]);
  end;

FUNCTION extractHsvChannels(CONST x:TVector3):TVector3;
  VAR brightChannel:byte;
  begin
    initialize(result);
    if x[0]>x[1]      then begin result[2]:=x[0]; brightChannel:=0; end
                      else begin result[2]:=x[1]; brightChannel:=1; end;
    if x[2]>result[2] then begin result[2]:=x[2]; brightChannel:=2; end;
    //result[2] now holds the brightest component of x
    if x[0]<x[1]      then result[1]:=x[0]
                      else result[1]:=x[1];
    if x[2]<result[1] then result[1]:=x[2];
    //result[1] now holds the darkest component of x
    case brightChannel of
      0 : result[0]:=(  (x[1]-x[2])/(result[2]-result[1]))/6;
      1 : result[0]:=(2+(x[2]-x[0])/(result[2]-result[1]))/6;
      2 : result[0]:=(4+(x[0]-x[1])/(result[2]-result[1]))/6;
    end;
    result[1]:=(result[2]-result[1])/result[2];
    while result[0]<0 do result[0]:=result[0]+1;
    while result[0]>1 do result[0]:=result[0]-1;
  end;

FUNCTION orthonormalBasisOf(CONST v1,v2:TVector3):TMatrix3x3;
  begin
    result[0]:=                                  v1*(1/euklideanNorm(v1));
    result[2]:=cross(result[0],v2); result[2]*=1/euklideanNorm(result[2]);
    result[1]:=cross(result[0],result[2]);
  end;

FUNCTION invert(CONST m:TMatrix3x3):TMatrix3x3;
  VAR invDet:Tfloat;
  begin
    invDet:=(m[0,0]*m[1,1]*m[2,2]
            +m[0,1]*m[1,2]*m[2,0]
            +m[0,2]*m[1,0]*m[2,1]
            -m[0,0]*m[1,2]*m[2,1]
            -m[0,1]*m[1,0]*m[2,2]
            -m[0,2]*m[1,1]*m[2,0]);
    if abs(invDet)>1E-10 then begin
      invDet:=1/invDet;
      result[0,0]:=invDet*(m[1,1]*m[2,2]-m[1,2]*m[2,1]);
      result[1,0]:=invDet*(m[1,2]*m[2,0]-m[1,0]*m[2,2]);
      result[2,0]:=invDet*(m[1,0]*m[2,1]-m[1,1]*m[2,0]);
      result[0,1]:=invDet*(m[2,1]*m[0,2]-m[2,2]*m[0,1]);
      result[1,1]:=invDet*(m[2,2]*m[0,0]-m[2,0]*m[0,2]);
      result[2,1]:=invDet*(m[2,0]*m[0,1]-m[2,1]*m[0,0]);
      result[0,2]:=invDet*(m[0,1]*m[1,2]-m[0,2]*m[1,1]);
      result[1,2]:=invDet*(m[0,2]*m[1,0]-m[0,0]*m[1,2]);
      result[2,2]:=invDet*(m[0,0]*m[1,1]-m[0,1]*m[1,0]);
    end else begin
      result[0,0]:=Nan;
      result[1,0]:=Nan;
      result[2,0]:=Nan;
      result[0,1]:=Nan;
      result[1,1]:=Nan;
      result[2,1]:=Nan;
      result[0,2]:=Nan;
      result[1,2]:=Nan;
      result[2,2]:=Nan;
    end;
  end;

FUNCTION transpose(CONST m:TMatrix3x3):TMatrix3x3;
  VAR i,j:longint;
  begin
    for i:=0 to 2 do for j:=0 to 2 do result[i,j]:=m[j,i];
  end;

OPERATOR * (CONST A:TMatrix3x3; CONST v:TVector3):TVector3;
  begin
    result[0]:=A[0,0]*v[0]+A[0,1]*v[1]+A[0,2]*v[2];
    result[1]:=A[1,0]*v[0]+A[1,1]*v[1]+A[1,2]*v[2];
    result[2]:=A[2,0]*v[0]+A[2,1]*v[1]+A[2,2]*v[2];
  end;

OPERATOR * (CONST x:TVector3x5; CONST y:Tfloat):TVector3x5;
  VAR i,j:longint;
  begin
    for i:=0 to 4 do for j:=0 to 2 do result[i,j]:=x[i,j]*y;
  end;

OPERATOR + (CONST x,y:TVector3x5):TVector3x5;
  VAR i,j:longint;
  begin
    for i:=0 to 4 do for j:=0 to 2 do result[i,j]:=x[i,j]+y[i,j];
  end;

FUNCTION maxNormDiff(CONST x,y:TVector3x5):Tfloat;
  begin
    result:=max(sqrEuklideanNorm(x[0]-y[0]),
                sqrEuklideanNorm(x[1]-y[1]));
    result:=max(sqrEuklideanNorm(x[2]-y[2]),result);
    result:=max(sqrEuklideanNorm(x[3]-y[3]),result);
    result:=max(sqrEuklideanNorm(x[4]-y[4]),result);
  end;

end.

