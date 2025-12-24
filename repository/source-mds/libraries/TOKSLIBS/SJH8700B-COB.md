# SJH8700B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SJH8700B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　ベンダーオンライン　　　　　　　　*
*    モジュール名　　　　：　ＪＥＤＩＣＯＳデータ変換　　　　　*
*    作成日／更新日　　　：　2007/05/15                        *
*    作成者／更新者　　　：　ＮＡＶ　松野　　　　　　　　　　　*
*    処理概要　　　　　　：　ＪＥＤＩＣＯＳにて受信したデータ　*
*                            をＪＣＡフォーマットに変換する。　*
*    作成日／更新日　　　：　2009/11/06                        *
*    作成者／更新者　　　：　ＮＡＶ　高橋　　　　　　　　　　　*
*    処理概要　　　　　　：　ダイキ内にサンコーを追加　　　　　*
*    作成日／更新日　　　：　2013/01/10                        *
*    作成者／更新者　　　：　ＮＡＶ　高橋　　　　　　　　　　　*
*    処理概要　　　　　　：　ダイキ⇒ルートＣＤセット　　　　　*
*    作成日／更新日　　　：　2014/08/19                        *
*    作成者／更新者　　　：　ＮＡＶ　高橋　　　　　　　　　　　*
*    処理概要　　　　　　：　ホーマック発注納品方法変更　　　　*
*    作成日／更新日　　　：　2016/02/09                        *
*    作成者／更新者　　　：　ＮＡＶ　高橋　　　　　　　　　　　*
*    処理概要　　　　　　：　カーマセンター納品対応　　　　　　*
*    作成日／更新日　　　：　2016/02/17                        *
*    作成者／更新者　　　：　ＮＡＶ　高橋　　　　　　　　　　　*
*    処理概要　　　　　　：　南アルプスセンター納品対応　　　　*
*    作成日／更新日　　　：　2016/06/05                        *
*    作成者／更新者　　　：　ＮＡＶ　高橋　　　　　　　　　　　*
*    処理概要　　　　　　：　ダイキ発注種別区分追加　　　　　　*
*    作成日／更新日　　　：　2018/02/13                        *
*    作成者／更新者　　　：　ＮＡＶ　高橋　　　　　　　　　　　*
*    処理概要　　　　　　：　ケーヨー資材／植物追加（東／西）　*
*    作成日／更新日　　　：　2019/02/21                        *
*    作成者／更新者　　　：　ＮＡＶ　高橋　　　　　　　　　　　*
*    処理概要　　　　　　：　ホーマック新発注種別区分対応　　　*
*    作成日／更新日　　　：　2019/03/11                        *
*    作成者／更新者　　　：　ＮＡＶ　高橋　　　　　　　　　　　*
*    処理概要　　　　　　：　発注種別区分マスタ参照へ変更　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SJH8700B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          07/05/15.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*受信データファイル
     SELECT   JEDICOS   ASSIGN    TO        DA-01-S-JEDICOS
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    EDI-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*基本情報データ
     SELECT   DJJOHOF   ASSIGN    TO        DA-01-VI-DJJOHOL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JOH-K01   JOH-K02
                                            JOH-K03   JOH-K04
                                            JOH-K05   JOH-K06
                                            JOH-K07   JOH-K08
                        FILE  STATUS   IS   JOH-STATUS.
*ＪＣＡフォーマット（ホーマック資材北海道分）
     SELECT   JCAFILE1   ASSIGN    TO        DA-01-S-JCAFILE1
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    JCA1-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ＪＣＡフォーマット（ホーマック資材仙台分）
     SELECT   JCAFILE2   ASSIGN    TO        DA-01-S-JCAFILE2
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    JCA2-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ＪＣＡフォーマット（ホーマック資材関東分）
     SELECT   JCAFILE3   ASSIGN    TO        DA-01-S-JCAFILE3
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    JCA3-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ＪＣＡフォーマット（カーマ資材分）
     SELECT   JCAKAMA1  ASSIGN    TO        DA-01-S-JCAKAMA1
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    KHM1-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ＪＣＡフォーマット（カーマ植物分）
     SELECT   JCAKAMA2  ASSIGN    TO        DA-01-S-JCAKAMA2
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    KHM2-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*##2017/02/17 NAV ST
*ＪＣＡフォーマット（カーマ資材分　南アルプス分）
     SELECT   JCAKAMA3  ASSIGN    TO        DA-01-S-JCAKAMA3
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    KHM3-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ＪＣＡフォーマット（カーマ植物分　南アルプス分）
     SELECT   JCAKAMA4  ASSIGN    TO        DA-01-S-JCAKAMA4
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    KHM4-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*##2017/02/17 NAV ED
*ＪＣＡフォーマット（ホーマック植物北海道分）
     SELECT   JCAFILE5  ASSIGN    TO        DA-01-S-JCAFILE5
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    JCA1-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ＪＣＡフォーマット（ホーマック植物仙台分）
     SELECT   JCAFILE6  ASSIGN    TO        DA-01-S-JCAFILE6
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    JCA2-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ＪＣＡフォーマット（ホーマック植物関東分）
     SELECT   JCAFILE7  ASSIGN    TO        DA-01-S-JCAFILE7
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    JCA3-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ＪＣＡフォーマット（ダイキ分）
     SELECT   JCADAIK1  ASSIGN    TO        DA-01-S-JCADAIK1
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    DAI1-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ＪＣＡフォーマット（ダイキ分）
     SELECT   JCADAIK2  ASSIGN    TO        DA-01-S-JCADAIK2
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    DAI2-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ＪＣＡフォーマット（ダイキ分）
     SELECT   JCADAIK3  ASSIGN    TO        DA-01-S-JCADAIK3
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    DAI3-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ＪＣＡフォーマット（ダイキ分）
     SELECT   JCADAIK4  ASSIGN    TO        DA-01-S-JCADAIK4
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    DAI4-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*#2018/02/13 NAV ST
*ＪＣＡフォーマット（ケーヨー資材東）
     SELECT   JCAKEYO1  ASSIGN    TO        DA-01-S-JCAKEYO1
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    KEI1-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ＪＣＡフォーマット（ケーヨー資材西）
     SELECT   JCAKEYO2  ASSIGN    TO        DA-01-S-JCAKEYO2
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    KEI2-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ＪＣＡフォーマット（ケーヨー植物東）
     SELECT   JCAKEYO3  ASSIGN    TO        DA-01-S-JCAKEYO3
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    KEI3-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ＪＣＡフォーマット（ケーヨー植物西）
     SELECT   JCAKEYO4  ASSIGN    TO        DA-01-S-JCAKEYO4
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    KEI4-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*#2018/02/13 NAV ED
*ＪＣＡフォーマット（対象外）
     SELECT   JCAFILE8  ASSIGN    TO        DA-01-S-JCAFILE8
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    JCA8-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*取引先マスタ
     SELECT   TOKMS2    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TOK-F01
                        FILE  STATUS   IS   TOK-STATUS.
*店舗マスタ
     SELECT   TENMS1    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TEN-F52 TEN-F011
                        FILE      STATUS    IS   TEN-STATUS.
*ルート条件マスタ
     SELECT   JHMRUTL1  ASSIGN    TO        DA-01-VI-JHMRUTL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       RUT-F01   RUT-F02
                                            RUT-F03
                        FILE STATUS    IS   RUT-STATUS.
*#2019/03/11 NAV ST
*発注種別変換マスタ
     SELECT   DCMHSBF   ASSIGN    TO        DA-01-VI-DCMHSBL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   HSB-F01 HSB-F02
                        FILE      STATUS    IS   HSB-STATUS.
*#2019/03/11 NAV ED
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    受信データ　ＲＬ＝　２５６　  ＢＦ＝　１
******************************************************************
 FD  JEDICOS
                        BLOCK CONTAINS      1    RECORDS
                        LABEL RECORD   IS   STANDARD.
*
 01  EDI-REC.
     03  EDI-01                  PIC  X(02).
     03  EDI-02                  PIC  X(3147).
******************************************************************
*    基本情報データ
******************************************************************
 FD  DJJOHOF            LABEL RECORD   IS   STANDARD.
     COPY     DJJOHOF   OF        XFDLIB
              JOINING   JOH       PREFIX.
******************************************************************
*    ＪＣＡフォーマットファイル（ホーマック資材北海道）
******************************************************************
 FD  JCAFILE1            LABEL RECORD   IS   STANDARD.
*
 01  JCA1-REC.
     03  FILLER                   PIC X(128).
******************************************************************
*    ＪＣＡフォーマットファイル（ホーマック資材東北）
******************************************************************
 FD  JCAFILE2            LABEL RECORD   IS   STANDARD.
*
 01  JCA2-REC.
     03  FILLER                   PIC X(128).
******************************************************************
*    ＪＣＡフォーマットファイル（ホーマック資材関東）
******************************************************************
 FD  JCAFILE3            LABEL RECORD   IS   STANDARD.
*
 01  JCA3-REC.
     03  FILLER                   PIC X(128).
******************************************************************
*    ＪＣＡフォーマットファイル（カーマ資材）
******************************************************************
 FD  JCAKAMA1            LABEL RECORD   IS   STANDARD.
*
 01  KHM1-REC.
     03  FILLER                   PIC X(128).
******************************************************************
*    ＪＣＡフォーマットファイル（カーマ植物）
******************************************************************
 FD  JCAKAMA2            LABEL RECORD   IS   STANDARD.
*
 01  KHM2-REC.
     03  FILLER                   PIC X(128).
*##2017/02/17 NAV ST
******************************************************************
*    ＪＣＡフォーマットファイル（カーマ資材）南アルプス対応
******************************************************************
 FD  JCAKAMA3            LABEL RECORD   IS   STANDARD.
*
 01  KHM3-REC.
     03  FILLER                   PIC X(128).
******************************************************************
*    ＪＣＡフォーマットファイル（カーマ植物）南アルプス対応
******************************************************************
 FD  JCAKAMA4            LABEL RECORD   IS   STANDARD.
*
 01  KHM4-REC.
     03  FILLER                   PIC X(128).
*##2017/02/17 NAV ED
******************************************************************
*    ＪＣＡフォーマットファイル（ホーマック植物北海道）
******************************************************************
 FD  JCAFILE5            LABEL RECORD   IS   STANDARD.
*
 01  JCA5-REC.
     03  FILLER                   PIC X(128).
******************************************************************
*    ＪＣＡフォーマットファイル（ホーマック植物東北）
******************************************************************
 FD  JCAFILE6            LABEL RECORD   IS   STANDARD.
*
 01  JCA6-REC.
     03  FILLER                   PIC X(128).
******************************************************************
*    ＪＣＡフォーマットファイル（ホーマック植物関東）
******************************************************************
 FD  JCAFILE7            LABEL RECORD   IS   STANDARD.
*
 01  JCA7-REC.
     03  FILLER                   PIC X(128).
******************************************************************
*    ＪＣＡフォーマットファイル（ダイキ）
******************************************************************
 FD  JCADAIK1            LABEL RECORD   IS   STANDARD.
*
 01  DAI1-REC.
     03  FILLER                   PIC X(128).
******************************************************************
*    ＪＣＡフォーマットファイル（ダイキ）
******************************************************************
 FD  JCADAIK2            LABEL RECORD   IS   STANDARD.
*
 01  DAI2-REC.
     03  FILLER                   PIC X(128).
******************************************************************
*    ＪＣＡフォーマットファイル（ダイキ）
******************************************************************
 FD  JCADAIK3            LABEL RECORD   IS   STANDARD.
*
 01  DAI3-REC.
     03  FILLER                   PIC X(128).
******************************************************************
*    ＪＣＡフォーマットファイル（ダイキ）
******************************************************************
 FD  JCADAIK4            LABEL RECORD   IS   STANDARD.
*
 01  DAI4-REC.
     03  FILLER                   PIC X(128).
*#2018/02/13 NAV ST
******************************************************************
*    ＪＣＡフォーマットファイル（ケーヨー資材東）
******************************************************************
 FD  JCAKEYO1            LABEL RECORD   IS   STANDARD.
*
 01  KEI1-REC.
     03  FILLER                   PIC X(200).
******************************************************************
*    ＪＣＡフォーマットファイル（ケーヨー資材西）
******************************************************************
 FD  JCAKEYO2            LABEL RECORD   IS   STANDARD.
*
 01  KEI2-REC.
     03  FILLER                   PIC X(200).
******************************************************************
*    ＪＣＡフォーマットファイル（ケーヨー植物東）
******************************************************************
 FD  JCAKEYO3            LABEL RECORD   IS   STANDARD.
*
 01  KEI3-REC.
     03  FILLER                   PIC X(200).
******************************************************************
*    ＪＣＡフォーマットファイル（ケーヨー植物西）
******************************************************************
 FD  JCAKEYO4            LABEL RECORD   IS   STANDARD.
*
 01  KEI4-REC.
     03  FILLER                   PIC X(200).
*#2018/02/13 NAV ED
******************************************************************
*    ＪＣＡフォーマットファイル（対象外）
******************************************************************
 FD  JCAFILE8            LABEL RECORD   IS   STANDARD.
*
 01  JCA8-REC.
     03  FILLER                   PIC X(128).
******************************************************************
*    取引先マスタ
******************************************************************
 FD  TOKMS2             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
******************************************************************
*    店舗マスタ
******************************************************************
 FD  TENMS1.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
******************************************************************
*    ルート条件マスタ
******************************************************************
 FD  JHMRUTL1           LABEL RECORD   IS   STANDARD.
     COPY     JHMRUTF   OF        XFDLIB
              JOINING   RUT       PREFIX.
*****************************************************************
*#2019/03/11 NAV ST **********************************************
*    発注種別変換マスタ
******************************************************************
 FD  DCMHSBF            LABEL RECORD   IS   STANDARD.
     COPY     DCMHSBF   OF        XFDLIB
              JOINING   HSB       PREFIX.
*#2019/03/11 NAV ED *********************************************
*
 WORKING-STORAGE        SECTION.
*ヘッダ情報格納領域
     COPY   DJHACHED OF XFDLIB  JOINING   HED  AS   PREFIX.
*ヘッダ情報格納領域
     COPY   DJHACMEI OF XFDLIB  JOINING   MEI  AS   PREFIX.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  IDX                     PIC  9(02)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  JOH-CNT                 PIC  9(08)     VALUE  ZERO.
 01  JCA-CNT1                PIC  9(08)     VALUE  ZERO.
 01  JCA-CNT2                PIC  9(08)     VALUE  ZERO.
 01  JCA-CNT3                PIC  9(08)     VALUE  ZERO.
 01  JCA-CNTKAMA1            PIC  9(08)     VALUE  ZERO.
 01  JCA-CNTKAMA2            PIC  9(08)     VALUE  ZERO.
*#2017/02/17 NAV ST
 01  JCA-CNTKAMA3            PIC  9(08)     VALUE  ZERO.
 01  JCA-CNTKAMA4            PIC  9(08)     VALUE  ZERO.
*#2017/02/17 NAV ED
 01  JCA-CNT5                PIC  9(08)     VALUE  ZERO.
 01  JCA-CNT6                PIC  9(08)     VALUE  ZERO.
 01  JCA-CNT7                PIC  9(08)     VALUE  ZERO.
 01  JCA-CNTDAIK1            PIC  9(08)     VALUE  ZERO.
 01  JCA-CNTDAIK2            PIC  9(08)     VALUE  ZERO.
 01  JCA-CNTDAIK3            PIC  9(08)     VALUE  ZERO.
 01  JCA-CNTDAIK4            PIC  9(08)     VALUE  ZERO.
 01  JCA-CNT8                PIC  9(08)     VALUE  ZERO.
*#2018/02/13 NAV ST
 01  JCA-CNTKEYO1            PIC  9(08)     VALUE  ZERO.
 01  JCA-CNTKEYO2            PIC  9(08)     VALUE  ZERO.
 01  JCA-CNTKEYO3            PIC  9(08)     VALUE  ZERO.
 01  JCA-CNTKEYO4            PIC  9(08)     VALUE  ZERO.
*#2018/02/13 NAV ED
 01  TOKMS2-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  HTENMS-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  HTENMS-CHK-FLG          PIC  X(03)     VALUE  SPACE.
 01  JHMRUTL1-INV-FLG        PIC  X(03)     VALUE  SPACE.
 01  DCMHSBF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  WK-TOK-F81              PIC  X(02)     VALUE  SPACE.
 01  WK-JOH-F17              PIC  9(02)     VALUE  ZERO.
 01  WK-JOH-F304             PIC  X(06)     VALUE  SPACE.
 01  WK-JOH-F21              PIC  9(04)     VALUE  ZERO.
 01  KETA-CNT                PIC  9(02)     VALUE  ZERO.
 01  I                       PIC  9(02)     VALUE  ZERO.
*#2019/03/11 NAV ST
 01  WK-TORIHIKISAKI         PIC  9(08)     VALUE  ZERO.
*#2019/03/11 NAV ED
*伝票番号変換（文字⇒文字）
 01  WK-DENNOX             PIC  X(09).
 01  WK-DENNOX-R           REDEFINES  WK-DENNOX.
     03  WK-HEN-DENNOX     OCCURS 9.
       05  WK-HEN-DENNOXX  PIC  X(01).
*伝票番号変換（文字⇒数値）
 01  WK-DENNO              PIC  X(09).
 01  WK-DENNO-R            REDEFINES  WK-DENNO.
     03  WK-HEN-DENNO      PIC  9(09).
*伝票番号変換（文字⇒数値）
 01  WK-DENNO1             PIC  X(09).
 01  WK-DENNO1-R           REDEFINES  WK-DENNO1.
     03  WK-HEN-DENNO1     PIC  X(03).
     03  WK-HEN-DENNO2     PIC  X(06).
*取引先ＣＤ変換（文字⇒数値）
 01  WK-TORICD-HEN         PIC  X(06).
 01  WK-TORICD-HEN-R       REDEFINES  WK-TORICD-HEN.
     03  WK-HEN-TORICD     PIC  9(06).
*
*ヘッドレコード退避ワーク（ホーマック用）
 01  WK-DEPB-REC.
     03  WK-DEPB01          PIC  X(01).
     03  WK-DEPB02          PIC  9(01).
     03  WK-DEPB03          PIC  9(07).
     03  WK-DEPB04          PIC  9(06).
     03  WK-DEPB05          PIC  9(06).
     03  WK-DEPB06.
         05  WK-DEPB061     PIC  9(06).
         05  WK-DEPB062     PIC  9(02).
     03  WK-DEPB07.
         05  WK-DEPB071     PIC  X(20).
         05  WK-DEPB072     PIC  X(20).
     03  WK-DEPB08.
         05  WK-DEPB081     PIC  9(04).
         05  WK-DEPB082     PIC  X(02).
     03  WK-DEPB09          PIC  X(15).
     03  WK-DEPB10.
         05  WK-DEPB101     PIC  9(03).
         05  WK-DEPB102     PIC  X(01).
     03  WK-DEPB11          PIC  9(02).
     03  WK-DEPB12          PIC  9(04).
     03  WK-DEPB13          PIC  9(02).
     03  WK-DEPB14          PIC  9(01).
     03  WK-DEPB15          PIC  9(01).
*2014/08/19 NAV ST ホーマック発注納品方法変更対応
*****03  WK-DEPB16          PIC  X(23).
     03  WK-DEPB16          PIC  X(18).
     03  WK-DEPB161         PIC  X(02).
     03  WK-DEPB162         PIC  X(01).
     03  WK-DEPB163         PIC  X(01).
     03  WK-DEPB164         PIC  X(01).
*2014/08/19 NAV ED
     03  WK-DEPB17          PIC  X(01).

*    明細レコード退避ワーク（ホーマック用）
 01  WK-DEPD-REC.
     03  WK-DEPD01          PIC  X(01).
     03  WK-DEPD02          PIC  9(01).
     03  WK-DEPD03.
         05  WK-DEPD031     PIC  9(07).
         05  WK-DEPD032     PIC  X(01).
     03  WK-DEPD04.
         05  WK-DEPD041     PIC  X(20).
         05  WK-DEPD042     PIC  X(20).
     03  WK-DEPD05          PIC  9(05)V9.
     03  WK-DEPD06          PIC  9(06)V99.
     03  WK-DEPD07          PIC  9(08).
     03  WK-DEPD08          PIC  9(06).
     03  WK-DEPD09          PIC  9(08).
     03  WK-DEPD10          PIC  X(13).
*2014/08/19 NAV ST ホーマック発注納品方法変更対応
*****03  WK-DEPD11          PIC  X(29).
     03  WK-DEPD11          PIC  X(27).
     03  WK-DEPD111         PIC  X(01).
     03  WK-DEPD112         PIC  X(01).
*2014/08/19 NAV ED
*    合計レコード退避ワーク（ホーマック用）
 01  WK-DEPT-REC.
     03  WK-DEPT01          PIC  X(01).
     03  WK-DEPT02          PIC  9(01).
     03  WK-DEPT03          PIC  9(08).
     03  WK-DEPT04          PIC  9(08).
     03  WK-DEPT05          PIC  X(110).
*
*ヘッダ情報退避ワーク（カーマ用）
 01  WK-KDH-REC.
     03  WK-KDH01           PIC       X(01).
     03  WK-KDH02           PIC       X(02).
     03  WK-KDH03           PIC       9(05).
     03  WK-KDH04           PIC       9(04).
     03  WK-KDH05           PIC       9(03).
     03  WK-KDH06           PIC       9(06).
     03  WK-KDH07           PIC       X(03).
     03  WK-KDH08           PIC       9(02).
     03  WK-KDH09           PIC       9(02).
     03  WK-KDH10           PIC       9(08).
     03  WK-KDH11           PIC       9(08).
     03  WK-KDH12           PIC       X(10).
     03  WK-KDH13           PIC       X(07).
     03  WK-KDH14           PIC       X(10).
     03  WK-KDH15           PIC       X(15).
     03  WK-KDH16           PIC       X(02).
     03  WK-KDH17           PIC       X(08).
     03  WK-KDH18           PIC       X(08).
     03  WK-KDH19           PIC       X(05).
     03  WK-KDH20           PIC       X(06).
     03  WK-KDH21           PIC       X(01).
     03  WK-KDH22           PIC       X(03).
*#2017/02/06 NAV ST
*****03  WK-KDH23           PIC       X(09).
     03  WK-KDH23           PIC       X(02).
     03  WK-KDH24           PIC       X(02).
     03  WK-KDH25           PIC       X(01).
     03  WK-KDH26           PIC       X(01).
     03  WK-KDH27           PIC       X(03).
*#2017/02/06 NAV ST
*
*    明細情報退避ワーク（カーマ用）
 01  WK-KDD-REC.
     03  WK-KDD01           PIC       X(01).
     03  WK-KDD02           PIC       9(02).
     03  WK-KDD03           PIC       9(03).
     03  WK-KDD04           PIC       9(06).
     03  WK-KDD05           PIC       9(02).
     03  WK-KDD06           PIC       9(07).
     03  WK-KDD07           PIC       9(01).
     03  WK-KDD08           PIC       X(20).
     03  WK-KDD09           PIC       X(20).
     03  WK-KDD10           PIC       X(13).
     03  WK-KDD11           PIC       9(05)V9(01).
     03  WK-KDD12           PIC       9(07)V9(02).
     03  WK-KDD13           PIC       9(07).
*#2016/02/07 NAV ST
*****03  WK-KDD14           PIC       X(31).
     03  WK-KDD14           PIC       X(25).
     03  WK-KDD15           PIC       X(01).
     03  WK-KDD16           PIC       X(01).
     03  WK-KDD17           PIC       9(04).
*#2016/02/07 NAV ED

*ヘッド情報退避ワーク（ダイキ用）
 01  WK-DDH-REC.
     03  WK-DDH01           PIC  X(01).
     03  WK-DDH02           PIC  X(02).
     03  WK-DDH03           PIC  9(09).
*2009/11/06 NAV ST *
*****03  WK-DDH04           PIC  X(06).
*****03  WK-DDH05           PIC  X(03).
     03  WK-DDH04           PIC  X(04).
     03  WK-DDH05           PIC  9(05).
*2009/11/06 NAV ED *
     03  WK-DDH06.
       05  WK-DDH061        PIC  X(01).
       05  WK-DDH062        PIC  X(03).
     03  WK-DDH07           PIC  9(02).
     03  WK-DDH08           PIC  9(06).
     03  WK-DDH09           PIC  9(06).
     03  WK-DDH10           PIC  9(06).
     03  WK-DDH11           PIC  X(02).
     03  WK-DDH12           PIC  X(15).
     03  WK-DDH13           PIC  X(15).
     03  WK-DDH14           PIC  X(15).
     03  WK-DDH15           PIC  X(15).
     03  WK-DDH16           PIC  9(06).
     03  WK-DDH17           PIC  9(04).
*#2017/06/05 NAV ST
*****03  WK-DDH18           PIC  X(10).
     03  WK-DDH18.
         05  WK-DDH181      PIC  X(02).
         05  WK-DDH182      PIC  X(02).
         05  WK-DDH183      PIC  X(01).
         05  WK-DDH184      PIC  X(03).
         05  WK-DDH185      PIC  X(01).
         05  WK-DDH186      PIC  X(01).
*#2017/06/05 NAV ED
     03  WK-DDH19           PIC  X(01).
*    明細情報退避ワーク（ダイキ用）
 01  WK-DDD-REC.
     03  WK-DDD01           PIC  X(01).
     03  WK-DDD02           PIC  X(02).
     03  WK-DDD03           PIC  9(02).
     03  WK-DDD04           PIC  9(13).
     03  WK-DDD05           PIC  9(04).
     03  WK-DDD06           PIC  9(04).
     03  WK-DDD07           PIC  X(02).
     03  WK-DDD08           PIC  9(05)V9(01).
     03  WK-DDD09           PIC  9(07)V9(02).
     03  WK-DDD10           PIC  9(07).
     03  WK-DDD11           PIC  9(10).
     03  WK-DDD12           PIC  9(10).
     03  WK-DDD13.
       05  WK-DDD131        PIC  X(01).
       05  WK-DDD132        PIC  X(08).
     03  WK-DDD14           PIC  X(20).
     03  WK-DDD15           PIC  X(20).
     03  WK-DDD16           PIC  X(01).
     03  WK-DDD17           PIC  X(03).
     03  WK-DDD18           PIC  9(03).
*#2017/06/05 NAV ST
*****03  WK-DDD19           PIC  X(02).
     03  WK-DDD19           PIC  X(01).
     03  WK-DDD20           PIC  X(01).
*#2017/06/05 NAV ED
*##2018/02/16 NAV ST
*ヘッダ情報退避ワーク（ケーヨー用）
 01  WK-KKDH-REC.
     03  WK-KKDH01      PIC   X(01). *>B
     03  WK-KKDH02      PIC   X(02). *>SPACE
     03  WK-KKDH03      PIC   9(05). *>取引先CD
     03  WK-KKDH04      PIC   9(04). *>0
     03  WK-KKDH05      PIC   9(03). *>納品先
     03  WK-KKDH06      PIC   9(06). *>伝票番号
     03  WK-KKDH07      PIC   X(03). *>SPACE
     03  WK-KKDH08      PIC   9(02). *>部門CD下２桁
     03  WK-KKDH09      PIC   9(02). *>0
     03  WK-KKDH10      PIC   9(08). *>発注日
     03  WK-KKDH11      PIC   9(08). *>納品日終了
     03  WK-KKDH12      PIC   X(10). *>取引先名カナ
     03  WK-KKDH13      PIC   X(07). *>取引先名カナ
     03  WK-KKDH14      PIC   X(10). *>店舗名カナ
     03  WK-KKDH15      PIC   X(15). *>SPACE
     03  WK-KKDH16      PIC   X(02). *>直送CD
     03  WK-KKDH17      PIC   X(08). *>納品開始日
     03  WK-KKDH18      PIC   X(08). *>納品終了日
     03  WK-KKDH19      PIC   X(05). *>SPACE
     03  WK-KKDH20      PIC   X(06). *>SPACE
     03  WK-KKDH21      PIC   X(01). *>SPACE
     03  WK-KKDH22      PIC   X(03). *>SPACE
     03  WK-KKDH23      PIC   X(02). *>SPACE
     03  WK-KKDH24      PIC   X(02). *>発注種別区分
     03  WK-KKDH25      PIC   X(01). *>発注種別区分変化
     03  WK-KKDH26      PIC   X(01). *>納品区分
     03  WK-KKDH27      PIC   X(03). *>部門CD
     03  WK-KKDH28      PIC   9(09). *>伝票番号
     03  WK-KKDH29      PIC   X(09). *>特売CD
     03  WK-KKDH30      PIC   X(02). *>拠点CD
     03  WK-KKDH31      PIC   X(02). *>館番号
     03  WK-KKDH32      PIC   9(06). *>共通取引先
     03  WK-KKDH33      PIC   X(06). *>直送先CD
     03  WK-KKDH34      PIC   X(09). *>申込番号
     03  WK-KKDH99      PIC   X(29). *>予備
*    明細情報退避ワーク（ケーヨー用）
 01  WK-KKDD-REC.
     03  WK-KKDD01      PIC   X(01). *>D
     03  WK-KKDD02      PIC   9(02). *>0
     03  WK-KKDD03      PIC   9(03). *>納品先CD
     03  WK-KKDD04      PIC   9(06). *>伝票番号
     03  WK-KKDD05      PIC   9(02). *>行番号
     03  WK-KKDD06      PIC   9(07). *>商品CD
     03  WK-KKDD07      PIC   9(01). *>0
     03  WK-KKDD08      PIC   X(20). *>商品名カナ
     03  WK-KKDD09      PIC   X(20). *>規格名カナ
     03  WK-KKDD10      PIC   X(13). *>JANCD
     03  WK-KKDD11      PIC   9(05)V9(01). *>発注数
     03  WK-KKDD12      PIC   9(07)V9(02). *>原価単価
     03  WK-KKDD13      PIC   9(07). *>売価単価
     03  WK-KKDD14      PIC   X(25). *>SPACE
     03  WK-KKDD15      PIC   X(01). *>形状区分
     03  WK-KKDD16      PIC   X(01). *>発注単位区分
     03  WK-KKDD17      PIC   9(04). *>納品先CD
     03  WK-KKDD99      PIC   X(72). *>予備
*##2018/02/16 NAV ED
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  EDI-STATUS        PIC  X(02).
     03  JOH-STATUS        PIC  X(02).
     03  JCA1-STATUS       PIC  X(02).
     03  JCA2-STATUS       PIC  X(02).
     03  JCA3-STATUS       PIC  X(02).
     03  KHM1-STATUS       PIC  X(02).
     03  KHM2-STATUS       PIC  X(02).
*#2017/02/17 NAV ST
     03  KHM3-STATUS       PIC  X(02).
     03  KHM4-STATUS       PIC  X(02).
*#2017/02/17 NAV ED
     03  JCA5-STATUS       PIC  X(02).
     03  JCA6-STATUS       PIC  X(02).
     03  JCA7-STATUS       PIC  X(02).
     03  DAI1-STATUS       PIC  X(02).
     03  DAI2-STATUS       PIC  X(02).
     03  DAI3-STATUS       PIC  X(02).
     03  DAI4-STATUS       PIC  X(02).
     03  JCA8-STATUS       PIC  X(02).
     03  TOK-STATUS        PIC  X(02).
     03  TEN-STATUS        PIC  X(02).
     03  RUT-STATUS        PIC  X(02).
*#2018/02/13 NAV ST
     03  KEI1-STATUS       PIC  X(02).
     03  KEI2-STATUS       PIC  X(02).
     03  KEI3-STATUS       PIC  X(02).
     03  KEI4-STATUS       PIC  X(02).
*#2018/02/13 NAV ED
*#2019/03/11 NAV ST
     03  HSB-STATUS        PIC  X(02).
*#2019/03/11 NAV ED
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SJH8700B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJH8700B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJH8700B".
         05  FILLER         PIC   X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  AB-FILE        PIC   X(08).
         05  FILLER         PIC   X(06)  VALUE " ST = ".
         05  AB-STS         PIC   X(02).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " INPUT = ".
         05  IN-CNT         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " OUTPUT= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-JDATE             PIC   9(08).
 01  PARA-JTIME             PIC   9(04).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-JDATE  PARA-JTIME.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JEDICOS.
     MOVE      "JEDICOS "   TO   AB-FILE.
     MOVE      EDI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DJJOHOF.
     MOVE      "DJJOHOF "   TO   AB-FILE.
     MOVE      JOH-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JCAFILE1.
     MOVE      "JCAFILE1"    TO   AB-FILE.
     MOVE      JCA1-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JCAFILE2.
     MOVE      "JCAFILE2"    TO   AB-FILE.
     MOVE      JCA2-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JCAFILE3.
     MOVE      "JCAFILE3"    TO   AB-FILE.
     MOVE      JCA3-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC6           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JCAKAMA1.
     MOVE      "JCAKAMA1"    TO   AB-FILE.
     MOVE      KHM1-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC7           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JCAFILE5.
     MOVE      "JCAFILE5"    TO   AB-FILE.
     MOVE      JCA5-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC8           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JCAFILE6.
     MOVE      "JCAFILE6"    TO   AB-FILE.
     MOVE      JCA6-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC9           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JCAFILE7.
     MOVE      "JCAFILE7"    TO   AB-FILE.
     MOVE      JCA7-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC10          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JCAKAMA2.
     MOVE      "JCAKAMA2"    TO   AB-FILE.
     MOVE      KHM2-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC11          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JCADAIK1.
     MOVE      "JCADAIK1"    TO   AB-FILE.
     MOVE      DAI1-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC12          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JCADAIK2.
     MOVE      "JCADAIK2"    TO   AB-FILE.
     MOVE      DAI2-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC13          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JCAFILE8.
     MOVE      "JCAFILE8"    TO   AB-FILE.
     MOVE      JCA8-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC13          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TOKMS2.
     MOVE      "TOKMS2  "   TO   AB-FILE.
     MOVE      TOK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC14          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TENMS1.
     MOVE      "TENMS1  "   TO   AB-FILE.
     MOVE      TEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC15          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JHMRUTL1.
     MOVE      "JHMRUTL1"   TO   AB-FILE.
     MOVE      RUT-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC16          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JCADAIK3.
     MOVE      "JCADAIK3"    TO   AB-FILE.
     MOVE      DAI3-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC17          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JCADAIK4.
     MOVE      "JCADAIK4"    TO   AB-FILE.
     MOVE      DAI4-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC18          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JCAKAMA3.
     MOVE      "JCAKAMA3"    TO   AB-FILE.
     MOVE      KHM3-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC19          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JCAKAMA4.
     MOVE      "JCAKAMA4"    TO   AB-FILE.
     MOVE      KHM4-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC20          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JCAKEYO1.
     MOVE      "JCAKEYO1"    TO   AB-FILE.
     MOVE      KEI1-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC21          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JCAKEYO2.
     MOVE      "JCAKEYO2"    TO   AB-FILE.
     MOVE      KEI2-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC22          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JCAKEYO3.
     MOVE      "JCAKEYO3"    TO   AB-FILE.
     MOVE      KEI3-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC23          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JCAKEYO4.
     MOVE      "JCAKEYO4"    TO   AB-FILE.
     MOVE      KEI4-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC24          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DCMHSBF.
     MOVE      "DCMHSBL1"    TO   AB-FILE.
     MOVE      HSB-STATUS    TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FG    =    9.
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     JEDICOS   TOKMS2  JHMRUTL1  TENMS1.
     OPEN     I-O       DJJOHOF.
     OPEN     OUTPUT    JCAFILE1  JCAFILE2  JCAFILE3  JCAKAMA1.
     OPEN     OUTPUT    JCAFILE5  JCAFILE6  JCAFILE7  JCAKAMA2.
     OPEN     OUTPUT    JCADAIK1  JCADAIK2  JCAFILE8.
     OPEN     OUTPUT    JCADAIK3  JCADAIK4.
*#2017/02/17 NAV ST
     OPEN     OUTPUT    JCAKAMA3  JCAKAMA4.
*#2017/02/17 NAV ED
*#2018/02/13 NAV ST
     OPEN     OUTPUT    JCAKEYO1  JCAKEYO2  JCAKEYO3  JCAKEYO4.
*#2018/02/13 NAV ED
*#2019/03/11 NAV ST
     OPEN     INPUT     DCMHSBF.
*#2019/03/11 NAV ED
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG       RD-CNT.
     MOVE     ZERO      TO        JOH-CNT      JCA-CNT1  JCA-CNT2
                                  JCA-CNT3     JCA-CNTKAMA1.
     MOVE     ZERO      TO        JCA-CNT5     JCA-CNT6
                                  JCA-CNT7     JCA-CNTKAMA2.
     MOVE     ZERO      TO        JCA-CNTDAIK1 JCA-CNT8.
     MOVE     ZERO      TO        JCA-CNTDAIK2.
     MOVE     ZERO      TO        JCA-CNTDAIK3.
     MOVE     ZERO      TO        JCA-CNTDAIK4.
*#2017/02/17 NAV ST
     MOVE     ZERO      TO        JCA-CNTKAMA3 JCA-CNTKAMA4.
*#2017/02/17 NAV ED
*#2018/02/13 NAV ST
     MOVE     ZERO      TO        JCA-CNTKEYO1 JCA-CNTKEYO2.
     MOVE     ZERO      TO        JCA-CNTKEYO3 JCA-CNTKEYO4.
*#2018/02/13 NAV ED
     MOVE     SPACE     TO        WK-DEPB-REC.
     INITIALIZE                   WK-DEPB-REC.
     MOVE     SPACE     TO        WK-DEPD-REC.
     INITIALIZE                   WK-DEPD-REC.
     MOVE     SPACE     TO        WK-DEPT-REC.
     INITIALIZE                   WK-DEPT-REC.
     DISPLAY "PARA-JDATE = " PARA-JDATE UPON CONS.
     DISPLAY "PARA-JTIME = " PARA-JTIME UPON CONS.
*
******************
*システム日付編集*
******************
     ACCEPT      SYS-DATE  FROM      DATE.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        SYS-DATE  TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE    ZERO           TO   SYS-DATEW
     END-IF.
*
*
     PERFORM  JEDICOS-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*
*    伝票ヘッダレコード
     IF    EDI-01  =   "HD"
***********ワークエリア初期化
           MOVE      SPACE       TO   HED-REC
           INITIALIZE                 HED-REC
***********ヘッダ情報→ワークにセット
           MOVE      EDI-REC     TO   HED-REC
***********基本情報データ作成
           MOVE      SPACE       TO   JOH-REC
           INITIALIZE                 JOH-REC
           MOVE      PARA-JDATE  TO   JOH-K01
           MOVE      PARA-JTIME  TO   JOH-K02
           MOVE      ZERO        TO   JOH-K03
           MOVE      HED-F21     TO   JOH-K05
***********MOVE      HED-F03     TO   JOH-K06
           MOVE      SPACE       TO   WK-DENNOX
           MOVE      ZERO        TO   WK-DENNO
           MOVE      9           TO   KETA-CNT
           MOVE      EDI-REC(15:9) TO   WK-DENNOX
           PERFORM   VARYING  I  FROM 9  BY  -1  UNTIL I < 1
                IF   WK-HEN-DENNOX(I)  NOT =  SPACE
                     MOVE WK-HEN-DENNOX(I) TO WK-DENNO(KETA-CNT:1)
                     ADD -1 TO KETA-CNT
                END-IF
           END-PERFORM
           MOVE      WK-HEN-DENNO TO  JOH-K06
***********MOVE      HED-F06     TO   JOH-K08
           MOVE      HED-F042    TO   JOH-K08
           MOVE      HED-REC     TO   JOH-K20
***********MOVE      JOH-K06     TO   JOH-F03
           MOVE      JOH-F304    TO   WK-JOH-F304

***********取引先コードにより処理を振り分けます。
           EVALUATE  WK-JOH-F304

***************ホーマック（資材）
               WHEN  000880
               WHEN  000882
               WHEN  000883
***************JCA FORMAT 初期化
               MOVE      SPACE       TO   WK-DEPB-REC
               INITIALIZE                 WK-DEPB-REC
               MOVE      "H"         TO   WK-DEPB01
               MOVE      ZERO        TO   WK-DEPB02
***************MOVE      HED-F03     TO   WK-DEPB03
***************MOVE      JOH-F03(3:7) TO   WK-DEPB03
               MOVE      JOH-K06(3:7) TO   WK-DEPB03
               MOVE      HED-F05     TO   WK-DEPB04
               MOVE      HED-F06     TO   WK-DEPB05
***************MOVE      HED-F27     TO   WK-DEPB061
               MOVE      HED-F304    TO   WK-DEPB061
***************MOVE      ZERO        TO   WK-DEPB062
               MOVE  HED-F31(1:2)    TO   WK-DEPB062
               MOVE      HED-F29     TO   WK-DEPB07
               MOVE      HED-F21     TO   WK-DEPB081
               MOVE      SPACE       TO   WK-DEPB082
               MOVE      HED-F22     TO   WK-DEPB09
               MOVE      HED-F14     TO   WK-DEPB101
               MOVE      SPACE       TO   WK-DEPB102
               MOVE      ZERO        TO   WK-DEPB11
               MOVE      ZERO        TO   WK-DEPB12
               MOVE      ZERO        TO   WK-DEPB13
               MOVE      HED-F09     TO   WK-DEPB14
               MOVE      HED-F10     TO   WK-DEPB15
               MOVE      SPACE       TO   WK-DEPB16
***************2014/08/19 NAV ST
               MOVE      HED-F09     TO   WK-DEPB161
*##2019/03/11  EVALUATE  HED-F09
*##2019/03/11      WHEN  "00" MOVE "0"    TO   WK-DEPB164
*##2019/03/11      WHEN  "01" MOVE "3"    TO   WK-DEPB164
*##2019/03/11      WHEN  "02" MOVE "2"    TO   WK-DEPB164
*##2019/03/11      WHEN  "03" MOVE "4"    TO   WK-DEPB164
*##2019/03/11      WHEN  "04" MOVE "6"    TO   WK-DEPB164
*#2019/02/21 NAV ST
*******************WHEN  "05" MOVE "6"    TO   WK-DEPB164
*##2019/03/11      WHEN  "05" MOVE "F"    TO   WK-DEPB164
*##2019/03/11      WHEN  "51" MOVE "A"    TO   WK-DEPB164
*##2019/03/11      WHEN  "52" MOVE "B"    TO   WK-DEPB164
*##2019/03/11      WHEN  "71" MOVE "C"    TO   WK-DEPB164
*##2019/03/11      WHEN  "82" MOVE "D"    TO   WK-DEPB164
*##2019/03/11      WHEN  "83" MOVE "E"    TO   WK-DEPB164
*#2019/02/21 NAV ED
*##2019/03/11      WHEN OTHER MOVE "0"    TO   WK-DEPB164
*##2019/03/11  END-EVALUATE
***************2014/08/19 NAV ED
*#2019/03/11 NAV ST 発注種別変換区分取得
               MOVE      JOH-F17     TO   WK-JOH-F17
               EVALUATE  WK-JOH-F17
                   WHEN  2
                         MOVE  883   TO   WK-TORIHIKISAKI
                   WHEN  3
                         MOVE  882   TO   WK-TORIHIKISAKI
                   WHEN  4
                         MOVE  880   TO   WK-TORIHIKISAKI
                   WHEN  OTHER
                         MOVE  880   TO   WK-TORIHIKISAKI
               END-EVALUATE
               MOVE      SPACE            TO HSB-REC
               INITIALIZE                    HSB-REC
               MOVE      WK-TORIHIKISAKI  TO HSB-F01
               MOVE      HED-F09          TO HSB-F02
               PERFORM  DCMHSBF-READ-SEC
******DISPLAY "A = " WK-TORIHIKISAKI ":" HED-F09 ":"
***************DCMHSBF-INV-FLG ":" HSB-F03 UPON CONS
               IF       DCMHSBF-INV-FLG = SPACE
                        MOVE   HSB-F03    TO WK-DEPB164
************************DISPLAY "REC = " HSB-REC UPON CONS
               ELSE
                        MOVE   "1"        TO WK-DEPB164
                        DISPLAY NC"発注種別未登録エラー"
                            " " NC"取引先＝" WK-TORIHIKISAKI
                            " " NC"発注種別区分＝" HED-F09
                              NC"未登録の発注種別区分を確認！"
               END-IF
*#2019/03/11 NAV ED 発注種別変換区分取得
               MOVE      SPACE       TO   WK-DEPB17
               MOVE      JOH-F17     TO   WK-JOH-F17
               EVALUATE  WK-JOH-F17
                   WHEN  2
                         MOVE   WK-DEPB-REC TO JCA1-REC
                         WRITE  JCA1-REC
                         ADD    1    TO   JCA-CNT1
                         MOVE  883 TO   JOH-K03
                   WHEN  3
                         MOVE   WK-DEPB-REC TO JCA2-REC
                         WRITE  JCA2-REC
                         ADD    1    TO   JCA-CNT2
                         MOVE  882   TO   JOH-K03
                   WHEN  4
                         MOVE   WK-DEPB-REC TO JCA3-REC
                         WRITE  JCA3-REC
                         MOVE  880   TO   JOH-K03
                         ADD    1    TO   JCA-CNT3
                   WHEN  OTHER
                         MOVE   WK-DEPB-REC TO JCA8-REC
                         WRITE  JCA8-REC
                         ADD    1    TO   JCA-CNT8
                         MOVE  880   TO   JOH-K03
               END-EVALUATE

***************ホーマック（植物）
               WHEN  001427
               WHEN  014272
               WHEN  014273
***************JCA FORMAT 初期化
               MOVE      SPACE       TO   WK-DEPB-REC
               INITIALIZE                 WK-DEPB-REC
               MOVE      "H"         TO   WK-DEPB01
               MOVE      ZERO        TO   WK-DEPB02
***************MOVE      HED-F03     TO   WK-DEPB03
***************MOVE      JOH-F03(3:7) TO   WK-DEPB03
               MOVE      JOH-K06(3:7) TO   WK-DEPB03
               MOVE      HED-F05     TO   WK-DEPB04
               MOVE      HED-F06     TO   WK-DEPB05
***************MOVE      HED-F27     TO   WK-DEPB061
               MOVE      HED-F304    TO   WK-DEPB061
***************MOVE      ZERO        TO   WK-DEPB062
               MOVE   HED-F31(1:2)   TO   WK-DEPB062
               MOVE      HED-F29     TO   WK-DEPB07
               MOVE      HED-F21     TO   WK-DEPB081
               MOVE      SPACE       TO   WK-DEPB082
               MOVE      HED-F22     TO   WK-DEPB09
               MOVE      HED-F14     TO   WK-DEPB101
               MOVE      SPACE       TO   WK-DEPB102
               MOVE      ZERO        TO   WK-DEPB11
               MOVE      ZERO        TO   WK-DEPB12
               MOVE      ZERO        TO   WK-DEPB13
               MOVE      HED-F09     TO   WK-DEPB14
               MOVE      HED-F10     TO   WK-DEPB15
               MOVE      SPACE       TO   WK-DEPB16
***************2014/08/19 NAV ST
               MOVE      HED-F09     TO   WK-DEPB161
*#2019/03/11   EVALUATE  HED-F09
*#2019/03/11       WHEN  "00" MOVE "0"    TO   WK-DEPB164
*#2019/03/11       WHEN  "01" MOVE "3"    TO   WK-DEPB164
*#2019/03/11       WHEN  "02" MOVE "2"    TO   WK-DEPB164
*#2019/03/11       WHEN  "03" MOVE "4"    TO   WK-DEPB164
*#2019/03/11       WHEN  "04" MOVE "6"    TO   WK-DEPB164
*#2019/02/21 NAV ST
*******************WHEN  "05" MOVE "6"    TO   WK-DEPB164
*#2019/03/11       WHEN  "05" MOVE "F"    TO   WK-DEPB164
*#2019/03/11       WHEN  "51" MOVE "A"    TO   WK-DEPB164
*#2019/03/11       WHEN  "52" MOVE "B"    TO   WK-DEPB164
*#2019/03/11       WHEN  "71" MOVE "C"    TO   WK-DEPB164
*#2019/03/11       WHEN  "82" MOVE "D"    TO   WK-DEPB164
*#2019/03/11       WHEN  "83" MOVE "E"    TO   WK-DEPB164
*#2019/02/21 NAV ED
*#2019/03/11       WHEN OTHER MOVE "0"    TO   WK-DEPB164
*#2019/03/11   END-EVALUATE
***************2014/08/19 NAV ED
*#2019/03/11 NAV ST 発注種別変換区分取得
               MOVE      JOH-F17     TO   WK-JOH-F17
               EVALUATE  WK-JOH-F17
                   WHEN  2
                         MOVE  14273 TO   WK-TORIHIKISAKI
                   WHEN  3
                         MOVE  14272 TO   WK-TORIHIKISAKI
                   WHEN  4
                         MOVE  1427  TO   WK-TORIHIKISAKI
                   WHEN  OTHER
                         MOVE  1427  TO   WK-TORIHIKISAKI
               END-EVALUATE
               MOVE      SPACE            TO HSB-REC
               INITIALIZE                    HSB-REC
               MOVE      WK-TORIHIKISAKI  TO HSB-F01
               MOVE      HED-F09          TO HSB-F02
               PERFORM  DCMHSBF-READ-SEC
               IF       DCMHSBF-INV-FLG = SPACE
                        MOVE   HSB-F03    TO WK-DEPB164
               ELSE
                        MOVE   "1"        TO WK-DEPB164
                        DISPLAY NC"発注種別未登録エラー"
                            " " NC"取引先＝" WK-TORIHIKISAKI
                            " " NC"発注種別区分＝" HED-F09
                             NC"未登録の発注種別区分を確認！"
               END-IF
*#2019/03/11 NAV ED 発注種別変換区分取得
               MOVE      SPACE       TO   WK-DEPB17
               MOVE      JOH-F17     TO   WK-JOH-F17
               EVALUATE  WK-JOH-F17
                   WHEN  2
                         MOVE   WK-DEPB-REC TO JCA5-REC
                         WRITE  JCA5-REC
                         ADD    1    TO   JCA-CNT5
                         MOVE  14273 TO   JOH-K03
                   WHEN  3
                         MOVE   WK-DEPB-REC TO JCA6-REC
                         WRITE  JCA6-REC
                         ADD    1    TO   JCA-CNT6
                         MOVE  14272 TO   JOH-K03
                   WHEN  4
                         MOVE   WK-DEPB-REC TO JCA7-REC
                         WRITE  JCA7-REC
                         MOVE  1427  TO   JOH-K03
                         ADD    1    TO   JCA-CNT7
                   WHEN  OTHER
                         MOVE   WK-DEPB-REC TO JCA8-REC
                         WRITE  JCA8-REC
                         ADD    1    TO   JCA-CNT8
                         MOVE  1427  TO   JOH-K03
               END-EVALUATE

***************カーマ（資材）
               WHEN  013938
               MOVE      SPACE       TO   WK-KDH-REC
               INITIALIZE                 WK-KDH-REC
               MOVE      "B"         TO   WK-KDH01
               MOVE      SPACE       TO   WK-KDH02
               MOVE      13938       TO   WK-KDH03
               MOVE      ZERO        TO   WK-KDH04
               MOVE      HED-F21(2:3) TO  WK-KDH05
*#2017/02/17 NAV ST
               MOVE      HED-F21      TO  WK-JOH-F21
*#2017/02/17 NAV ED
***************MOVE      HED-F03(4:6) TO  WK-KDH06
***************MOVE      JOH-F03(4:6) TO  WK-KDH06
               MOVE      JOH-K06(4:6) TO  WK-KDH06
               MOVE      SPACE       TO   WK-KDH07
               MOVE      HED-F14(2:2) TO  WK-KDH08
               MOVE      ZERO        TO   WK-KDH09
               MOVE      HED-F05     TO   WK-KDH10
***************MOVE      HED-F06     TO   WK-KDH11
***************2007/07/23 納品終了日に変更する。
               MOVE      HED-F042    TO   WK-KDH11
               MOVE      HED-F29(1:10) TO WK-KDH12
               MOVE      HED-F202(1:7) TO WK-KDH13
               MOVE      HED-F22(1:10) TO WK-KDH14
               MOVE      SPACE       TO   WK-KDH15
***************MOVE      SPACE       TO   WK-KDH16
               MOVE    HED-F31(1:2)  TO   WK-KDH16
               MOVE      HED-F041    TO   WK-KDH17
               MOVE      HED-F042    TO   WK-KDH18
               MOVE      SPACE       TO   WK-KDH19
               MOVE      SPACE       TO   WK-KDH20
               MOVE      SPACE       TO   WK-KDH21
               MOVE      SPACE       TO   WK-KDH22
               MOVE      SPACE       TO   WK-KDH23
**************#2017/02/09 NAV ST
               MOVE      HED-F09     TO   WK-KDH24
*#2019/03/11   EVALUATE  HED-F09
*#2019/03/11       WHEN  "00" MOVE "0"    TO   WK-KDH25
*#2019/03/11       WHEN  "01" MOVE "3"    TO   WK-KDH25
*#2019/03/11       WHEN  "02" MOVE "2"    TO   WK-KDH25
*#2019/03/11       WHEN  "03" MOVE "4"    TO   WK-KDH25
*#2019/03/11       WHEN  "05" MOVE "6"    TO   WK-KDH25
*#2019/03/11       WHEN  "51" MOVE "8"    TO   WK-KDH25
*#2019/03/11       WHEN  "52" MOVE "7"    TO   WK-KDH25
*#2019/03/11       WHEN  "71" MOVE "9"    TO   WK-KDH25
*#2019/03/11       WHEN  "81" MOVE "A"    TO   WK-KDH25
*#2019/03/11       WHEN  "82" MOVE "1"    TO   WK-KDH25
*#2019/03/11       WHEN  "83" MOVE "B"    TO   WK-KDH25
*#2019/03/11       WHEN OTHER MOVE "0"    TO   WK-KDH25
*#2019/03/11   END-EVALUATE
               MOVE      HED-F10     TO   WK-KDH26
               MOVE      HED-F14     TO   WK-KDH27
***************2017/02/09 NAV ED
***************2017/02/17 NAV ST
************   MOVE      WK-KDH-REC  TO   KHM1-REC
*              WRITE     KHM1-REC
*              ADD       1           TO   JCA-CNTKAMA1
************   MOVE      13938       TO   JOH-K03
***************店舗マスタ索引
               MOVE      SPACE       TO   TEN-REC
               INITIALIZE                 TEN-REC
               MOVE 13938            TO   TEN-F52
               MOVE WK-JOH-F21       TO   TEN-F011
               PERFORM HTENMS-READ-SEC
               IF   HTENMS-CHK-FLG = SPACE
***************#2019/03/11 NAV ST
                    MOVE      SPACE       TO  HSB-REC
                    INITIALIZE                HSB-REC
                    MOVE      13938       TO  HSB-F01
                    MOVE      HED-F09     TO  HSB-F02
                    PERFORM  DCMHSBF-READ-SEC
                    IF   DCMHSBF-INV-FLG = SPACE
                         MOVE  HSB-F03    TO  WK-KDH25
                    ELSE
                         MOVE  "1"        TO  WK-KDH25
                        DISPLAY NC"発注種別未登録エラー"
                            " " NC"取引先＝" HSB-F01
                            " " NC"発注種別区分＝" HSB-F02
                              NC"未登録の発注種別区分を確認！"
                    END-IF
***************#2019/03/11 NAV ED
                    MOVE      WK-KDH-REC  TO   KHM1-REC
                    WRITE     KHM1-REC
                    ADD       1           TO   JCA-CNTKAMA1
                    MOVE      13938       TO   JOH-K03
               ELSE
***************#2019/03/11 NAV ST
                    MOVE      SPACE       TO  HSB-REC
                    INITIALIZE                HSB-REC
                    MOVE      139381      TO  HSB-F01
                    MOVE      HED-F09     TO  HSB-F02
                    PERFORM  DCMHSBF-READ-SEC
                    IF   DCMHSBF-INV-FLG = SPACE
                         MOVE  HSB-F03    TO  WK-KDH25
                    ELSE
                         MOVE  "1"        TO  WK-KDH25
                        DISPLAY NC"発注種別未登録エラー"
                            " " NC"取引先＝" HSB-F01
                            " " NC"発注種別区分＝" HSB-F02
                              NC"未登録の発注種別区分を確認！"
                    END-IF
***************#2019/03/11 NAV ED
                    MOVE      WK-KDH-REC  TO   KHM3-REC
                    WRITE     KHM3-REC
                    ADD       1           TO   JCA-CNTKAMA3
                    MOVE      139381      TO   JOH-K03
               END-IF
***************2017/02/17 NAV ED

***************カーマ（植物）
               WHEN  017137
               MOVE      SPACE       TO   WK-KDH-REC
               INITIALIZE                 WK-KDH-REC
               MOVE      "B"         TO   WK-KDH01
               MOVE      SPACE       TO   WK-KDH02
               MOVE      17137       TO   WK-KDH03
               MOVE      ZERO        TO   WK-KDH04
               MOVE      HED-F21(2:3) TO  WK-KDH05
*#2017/02/17 NAV ST
               MOVE      HED-F21      TO  WK-JOH-F21
*#2017/02/17 NAV ED
***************MOVE      HED-F03(4:6) TO  WK-KDH06
***************MOVE      JOH-F03(4:6) TO  WK-KDH06
               MOVE      JOH-K06(4:6) TO  WK-KDH06
               MOVE      SPACE       TO   WK-KDH07
               MOVE      HED-F14(2:2) TO  WK-KDH08
               MOVE      ZERO        TO   WK-KDH09
               MOVE      HED-F05     TO   WK-KDH10
***************MOVE      HED-F06     TO   WK-KDH11
***************2007/07/23 納品終了日に変更する。
               MOVE      HED-F042    TO   WK-KDH11
               MOVE      HED-F29(1:10) TO WK-KDH12
               MOVE      HED-F202(1:7) TO WK-KDH13
               MOVE      HED-F22(1:10) TO WK-KDH14
               MOVE      SPACE       TO   WK-KDH15
***************MOVE      SPACE       TO   WK-KDH16
               MOVE    HED-F31(1:2)  TO   WK-KDH16
               MOVE      HED-F041    TO   WK-KDH17
               MOVE      HED-F042    TO   WK-KDH18
               MOVE      SPACE       TO   WK-KDH19
               MOVE      SPACE       TO   WK-KDH20
               MOVE      SPACE       TO   WK-KDH21
               MOVE      SPACE       TO   WK-KDH22
               MOVE      SPACE       TO   WK-KDH23
**************#2017/02/09 NAV ST
               MOVE      HED-F09     TO   WK-KDH24
*#2019/03/11   EVALUATE  HED-F09
*#2019/03/11       WHEN  "00" MOVE "0"    TO   WK-KDH25
*#2019/03/11       WHEN  "01" MOVE "3"    TO   WK-KDH25
*#2019/03/11       WHEN  "02" MOVE "2"    TO   WK-KDH25
*#2019/03/11       WHEN  "03" MOVE "4"    TO   WK-KDH25
*#2019/03/11       WHEN  "05" MOVE "6"    TO   WK-KDH25
*#2019/03/11       WHEN  "51" MOVE "8"    TO   WK-KDH25
*#2019/03/11       WHEN  "52" MOVE "7"    TO   WK-KDH25
*#2019/03/11       WHEN  "71" MOVE "9"    TO   WK-KDH25
*#2019/03/11       WHEN  "81" MOVE "A"    TO   WK-KDH25
*#2019/03/11       WHEN  "82" MOVE "1"    TO   WK-KDH25
*#2019/03/11       WHEN  "83" MOVE "B"    TO   WK-KDH25
*#2019/03/11       WHEN OTHER MOVE "0"    TO   WK-KDH25
*#2019/03/11   END-EVALUATE
               MOVE      HED-F10     TO   WK-KDH26
               MOVE      HED-F14     TO   WK-KDH27
***************2017/02/09 NAV ED
***************2017/02/17 NAV ST
***************MOVE      WK-KDH-REC  TO   KHM2-REC
*              WRITE     KHM2-REC
*              ADD       1           TO   JCA-CNTKAMA2
***************MOVE      17137       TO   JOH-K03
***************店舗マスタ索引
               MOVE      SPACE       TO   TEN-REC
               INITIALIZE                 TEN-REC
               MOVE 17137            TO   TEN-F52
               MOVE WK-JOH-F21       TO   TEN-F011
               PERFORM HTENMS-READ-SEC
               IF   HTENMS-CHK-FLG = SPACE
***************#2019/03/11 NAV ST
                    MOVE      SPACE       TO  HSB-REC
                    INITIALIZE                HSB-REC
                    MOVE      17137       TO  HSB-F01
                    MOVE      HED-F09     TO  HSB-F02
                    PERFORM  DCMHSBF-READ-SEC
                    IF   DCMHSBF-INV-FLG = SPACE
                         MOVE  HSB-F03    TO  WK-KDH25
                    ELSE
                         MOVE  "1"        TO  WK-KDH25
                        DISPLAY NC"発注種別未登録エラー"
                            " " NC"取引先＝" HSB-F01
                            " " NC"発注種別区分＝" HSB-F02
                              NC"未登録の発注種別区分を確認！"
                    END-IF
***************#2019/03/11 NAV ED
                    MOVE      WK-KDH-REC  TO   KHM2-REC
                    WRITE     KHM2-REC
                    ADD       1           TO   JCA-CNTKAMA2
                    MOVE      17137       TO   JOH-K03
               ELSE
***************#2019/03/11 NAV ST
                    MOVE      SPACE       TO  HSB-REC
                    INITIALIZE                HSB-REC
                    MOVE      171371      TO  HSB-F01
                    MOVE      HED-F09     TO  HSB-F02
                    PERFORM  DCMHSBF-READ-SEC
                    IF   DCMHSBF-INV-FLG = SPACE
                         MOVE  HSB-F03    TO  WK-KDH25
                    ELSE
                         MOVE  "1"        TO  WK-KDH25
                        DISPLAY NC"発注種別未登録エラー"
                            " " NC"取引先＝" HSB-F01
                            " " NC"発注種別区分＝" HSB-F02
                              NC"未登録の発注種別区分を確認！"
                    END-IF
***************#2019/03/11 NAV ED
                    MOVE      WK-KDH-REC  TO   KHM4-REC
                    WRITE     KHM4-REC
                    ADD       1           TO   JCA-CNTKAMA4
                    MOVE      171371      TO   JOH-K03
               END-IF
***************2017/02/17 NAV ED

***************ダイキ
***************WHEN  100403
***************WHEN  100403  WHEN  100441
               WHEN  100403  WHEN  100441
               MOVE      SPACE       TO   WK-DDH-REC
               INITIALIZE                 WK-DDH-REC
               MOVE      "B"         TO   WK-DDH01
               MOVE      "01"        TO   WK-DDH02
               MOVE      HED-F03     TO   WK-DDH03
               MOVE      ZERO        TO   WK-DDH04
*2009/11/06 NAV ST *
***************MOVE      HED-F21(2:3) TO  WK-DDH05
               MOVE      HED-F21      TO  WK-DDH05
*2009/11/06 NAV ED *
               MOVE      ZERO        TO   WK-DDH061
               MOVE      HED-F14     TO   WK-DDH062
               MOVE      10          TO   WK-DDH07
               MOVE      HED-F05     TO   WK-DDH08
               MOVE      HED-F042    TO   WK-DDH09
***************MOVE      100403      TO   WK-DDH10
               MOVE      WK-JOH-F304 TO   WK-TORICD-HEN
               MOVE  WK-HEN-TORICD   TO   WK-DDH10
               MOVE      ZERO        TO   WK-DDH11
               MOVE      HED-F29(1:15) TO WK-DDH12
               MOVE      HED-F29(16:15) TO WK-DDH13
               MOVE      HED-F202(1:15) TO WK-DDH14
               MOVE      HED-F22     TO   WK-DDH15
               MOVE      HED-F042    TO   WK-DDH16
               MOVE      ZERO        TO   WK-DDH17
*2013/01/10 NAV ST セット項目を変更　ルートＣＤ
***************MOVE      SPACE       TO   WK-DDH18
*#2017/06/05 NAV ST
***************MOVE      HED-F31(1:2) TO  WK-DDH18(1:2)
               MOVE      HED-F31(1:2) TO  WK-DDH181
               MOVE      HED-F09      TO  WK-DDH182
               MOVE      HED-F10      TO  WK-DDH183
               MOVE      HED-F14      TO  WK-DDH184
               MOVE      SPACE        TO  WK-DDH185
               MOVE      SPACE        TO  WK-DDH186
*#2017/06/05 NAV ED
*2013/01/10 NAV ED セット項目を変更　ルートＣＤ
***************MOVE      SPACE       TO   WK-DDH19
               MOVE      HED-F09     TO   WK-DDH19
*#2017/06/05 NAV ST
*              EVALUATE  HED-F09
*                  WHEN  "00" MOVE "0"    TO   WK-DDH19
*                  WHEN  "01" MOVE "3"    TO   WK-DDH19
*                  WHEN  "02" MOVE "2"    TO   WK-DDH19
*                  WHEN  "03" MOVE "4"    TO   WK-DDH19
*                  WHEN  "82" MOVE "1"    TO   WK-DDH19
*                  WHEN  "05" MOVE "6"    TO   WK-DDH19
*                  WHEN OTHER MOVE "0"    TO   WK-DDH19
*****          END-EVALUATE
*#2019/03/11   EVALUATE  HED-F09
*#2019/03/11       WHEN  "00" MOVE "0"    TO   WK-DDH19
*#2019/03/11       WHEN  "01" MOVE "3"    TO   WK-DDH19
*#2019/03/11       WHEN  "02" MOVE "2"    TO   WK-DDH19
*#2019/03/11       WHEN  "03" MOVE "4"    TO   WK-DDH19
*#2019/03/11       WHEN  "05" MOVE "6"    TO   WK-DDH19
*#2019/03/11       WHEN  "51" MOVE "8"    TO   WK-DDH19
*#2019/03/11       WHEN  "52" MOVE "7"    TO   WK-DDH19
*#2019/03/11       WHEN  "71" MOVE "9"    TO   WK-DDH19
*#2019/03/11       WHEN  "81" MOVE "A"    TO   WK-DDH19
*#2019/03/11       WHEN  "82" MOVE "1"    TO   WK-DDH19
*#2019/03/11       WHEN  "83" MOVE "B"    TO   WK-DDH19
*#2019/03/11       WHEN OTHER MOVE "0"    TO   WK-DDH19
*#2019/03/11   END-EVALUATE
               MOVE WK-DDH19         TO   WK-DDH185
*#2017/06/05 NAV ED
***************店舗マスタ索引
               MOVE      SPACE       TO   TEN-REC
               INITIALIZE                 TEN-REC
               MOVE WK-HEN-TORICD    TO   TEN-F52
               MOVE WK-DDH05         TO   TEN-F011
               PERFORM HTENMS-READ-SEC
               IF   HTENMS-CHK-FLG = SPACE
***************#2019/03/11 NAV ST
                    MOVE  SPACE           TO   HSB-REC
                    INITIALIZE                 HSB-REC
                    MOVE  WK-HEN-TORICD   TO   HSB-F01
                    MOVE  HED-F09         TO   HSB-F02
                    PERFORM DCMHSBF-READ-SEC
                    IF  DCMHSBF-INV-FLG  =  SPACE
                        MOVE  HSB-F03     TO   WK-DDH19
                    ELSE
                        MOVE  "1"         TO   WK-DDH19
                        DISPLAY NC"発注種別未登録エラー"
                            " " NC"取引先＝" HSB-F01
                            " " NC"発注種別区分＝" HSB-F02
                              NC"未登録の発注種別区分を確認！"
                    END-IF
***************#2019/03/11 NAV ED
                    MOVE      WK-DDH-REC  TO   DAI1-REC
                    WRITE     DAI1-REC
                    ADD       1           TO   JCA-CNTDAIK1
                    MOVE  WK-HEN-TORICD   TO   JOH-K03
               ELSE
***************#2019/03/11 NAV ST
                    MOVE  SPACE           TO   HSB-REC
                    INITIALIZE                 HSB-REC
                    COMPUTE HSB-F01 = WK-HEN-TORICD + 1
                    MOVE  HED-F09         TO   HSB-F02
                    PERFORM DCMHSBF-READ-SEC
                    IF  DCMHSBF-INV-FLG  =  SPACE
                        MOVE  HSB-F03     TO   WK-DDH19
                    ELSE
                        MOVE  "1"         TO   WK-DDH19
                        DISPLAY NC"発注種別未登録エラー"
                            " " NC"取引先＝" HSB-F01
                            " " NC"発注種別区分＝" HSB-F02
                              NC"未登録の発注種別区分を確認！"
                    END-IF
***************#2019/03/11 NAV ED
                    MOVE      WK-DDH-REC  TO   DAI3-REC
                    WRITE     DAI3-REC
                    ADD       1           TO   JCA-CNTDAIK3
********************サンコー分は、実際の取引先ＣＤ＋１
                    COMPUTE JOH-K03 = WK-HEN-TORICD + 1
               END-IF
               WHEN  100427
               MOVE      SPACE       TO   WK-DDH-REC
               INITIALIZE                 WK-DDH-REC
               MOVE      "B"         TO   WK-DDH01
               MOVE      "01"        TO   WK-DDH02
               MOVE      HED-F03     TO   WK-DDH03
               MOVE      ZERO        TO   WK-DDH04
*2009/11/06 NAV ST *
***************MOVE      HED-F21(2:3) TO  WK-DDH05
               MOVE      HED-F21      TO  WK-DDH05
*2009/11/06 NAV ED *
               MOVE      ZERO        TO   WK-DDH061
               MOVE      HED-F14     TO   WK-DDH062
               MOVE      10          TO   WK-DDH07
               MOVE      HED-F05     TO   WK-DDH08
               MOVE      HED-F042    TO   WK-DDH09
***************MOVE      100403      TO   WK-DDH10
               MOVE      WK-JOH-F304 TO   WK-TORICD-HEN
               MOVE  WK-HEN-TORICD   TO   WK-DDH10
               MOVE      ZERO        TO   WK-DDH11
               MOVE      HED-F29(1:15) TO WK-DDH12
               MOVE      HED-F29(16:15) TO WK-DDH13
               MOVE      HED-F202(1:15) TO WK-DDH14
               MOVE      HED-F22     TO   WK-DDH15
               MOVE      HED-F042    TO   WK-DDH16
               MOVE      ZERO        TO   WK-DDH17
*2013/01/10 NAV ST セット項目を変更　ルートＣＤ
***************MOVE      SPACE       TO   WK-DDH18
*#2017/06/05 NAV ST
***************MOVE      HED-F31(1:2) TO  WK-DDH18(1:2)
               MOVE      HED-F31(1:2) TO  WK-DDH181
               MOVE      HED-F09      TO  WK-DDH182
               MOVE      HED-F10      TO  WK-DDH183
               MOVE      HED-F14      TO  WK-DDH184
               MOVE      SPACE        TO  WK-DDH185
               MOVE      SPACE        TO  WK-DDH186
*#2017/06/05 NAV ED
*2013/01/10 NAV ED セット項目を変更　ルートＣＤ
***************MOVE      SPACE       TO   WK-DDH19
               MOVE      HED-F09     TO   WK-DDH19
*#2016/06/05 NAV ST
*              EVALUATE  HED-F09
*                  WHEN  "00" MOVE "0"    TO   WK-DDH19
*                  WHEN  "01" MOVE "3"    TO   WK-DDH19
*                  WHEN  "02" MOVE "2"    TO   WK-DDH19
*                  WHEN  "03" MOVE "4"    TO   WK-DDH19
*                  WHEN  "82" MOVE "1"    TO   WK-DDH19
*                  WHEN  "05" MOVE "6"    TO   WK-DDH19
*                  WHEN OTHER MOVE "0"    TO   WK-DDH19
*              END-EVALUATE
*#2019/03/11   EVALUATE  HED-F09
*#2019/03/11       WHEN  "00" MOVE "0"    TO   WK-DDH19
*#2019/03/11       WHEN  "01" MOVE "3"    TO   WK-DDH19
*#2019/03/11       WHEN  "02" MOVE "2"    TO   WK-DDH19
*#2019/03/11       WHEN  "03" MOVE "4"    TO   WK-DDH19
*#2019/03/11       WHEN  "05" MOVE "6"    TO   WK-DDH19
*#2019/03/11       WHEN  "51" MOVE "8"    TO   WK-DDH19
*#2019/03/11       WHEN  "52" MOVE "7"    TO   WK-DDH19
*#2019/03/11       WHEN  "71" MOVE "9"    TO   WK-DDH19
*#2019/03/11       WHEN  "81" MOVE "A"    TO   WK-DDH19
*#2019/03/11       WHEN  "82" MOVE "1"    TO   WK-DDH19
*#2019/03/11       WHEN  "83" MOVE "B"    TO   WK-DDH19
*#2019/03/11       WHEN OTHER MOVE "0"    TO   WK-DDH19
*#2019/03/11   END-EVALUATE
               MOVE WK-DDH19         TO   WK-DDH185
*#2017/06/05 NAV ED
*##            MOVE      WK-DDH-REC  TO   DAI2-REC
*##            WRITE     DAI2-REC
*##            ADD       1           TO   JCA-CNTDAIK2
*********** ST 2009/07/30 データ上の取引先ＣＤを資料へ変更
***************MOVE      1004031     TO   JOH-K03
*##            MOVE  WK-HEN-TORICD   TO   JOH-K03
*********** ST 2009/07/30 データ上の取引先ＣＤを資料へ変更
***************店舗マスタ索引
               MOVE      SPACE       TO   TEN-REC
               INITIALIZE                 TEN-REC
               MOVE WK-HEN-TORICD    TO   TEN-F52
               MOVE WK-DDH05         TO   TEN-F011
               PERFORM HTENMS-READ-SEC
               IF   HTENMS-CHK-FLG = SPACE
***************#2019/03/11 NAV ST
                    MOVE  SPACE           TO   HSB-REC
                    INITIALIZE                 HSB-REC
                    MOVE  WK-HEN-TORICD   TO   HSB-F01
                    MOVE  HED-F09         TO   HSB-F02
                    PERFORM DCMHSBF-READ-SEC
                    IF  DCMHSBF-INV-FLG  =  SPACE
                        MOVE  HSB-F03     TO   WK-DDH19
                    ELSE
                        MOVE  "1"         TO   WK-DDH19
                        DISPLAY NC"発注種別未登録エラー"
                            " " NC"取引先＝" HSB-F01
                            " " NC"発注種別区分＝" HSB-F02
                              NC"未登録の発注種別区分を確認！"
                    END-IF
***************#2019/03/11 NAV ED
                    MOVE      WK-DDH-REC  TO   DAI2-REC
                    WRITE     DAI2-REC
                    ADD       1           TO   JCA-CNTDAIK2
                    MOVE  WK-HEN-TORICD   TO   JOH-K03
               ELSE
***************#2019/03/11 NAV ST
                    MOVE  SPACE           TO   HSB-REC
                    INITIALIZE                 HSB-REC
                    COMPUTE HSB-F01 = WK-HEN-TORICD + 1
                    MOVE  HED-F09         TO   HSB-F02
                    PERFORM DCMHSBF-READ-SEC
                    IF  DCMHSBF-INV-FLG  =  SPACE
                        MOVE  HSB-F03     TO   WK-DDH19
                    ELSE
                        MOVE  "1"         TO   WK-DDH19
                        DISPLAY NC"発注種別未登録エラー"
                            " " NC"取引先＝" HSB-F01
                            " " NC"発注種別区分＝" HSB-F02
                              NC"未登録の発注種別区分を確認！"
                    END-IF
***************#2019/03/11 NAV ED
                    MOVE      WK-DDH-REC  TO   DAI4-REC
                    WRITE     DAI4-REC
                    ADD       1           TO   JCA-CNTDAIK4
********************サンコー分は、実際の取引先ＣＤ＋１
                    COMPUTE JOH-K03 = WK-HEN-TORICD + 1
               END-IF

*#2018/02/13 NAV END  ｹｰﾖｰ##########################
**************ケーヨー追加（資材）
               WHEN  000173
               MOVE      SPACE         TO   WK-KKDH-REC
               INITIALIZE                   WK-KKDH-REC
               MOVE      "B"           TO   WK-KKDH01
               MOVE      SPACE         TO   WK-KKDH02
               MOVE      173           TO   WK-KKDH03
               MOVE      ZERO          TO   WK-KKDH04
               MOVE      HED-F21(2:3)  TO   WK-KKDH05
               MOVE      HED-F21       TO   WK-JOH-F21
               MOVE      JOH-K06(4:6)  TO   WK-KKDH06
               MOVE      SPACE         TO   WK-KKDH07
               MOVE      HED-F14(2:2)  TO   WK-KKDH08
               MOVE      ZERO          TO   WK-KKDH09
               MOVE      HED-F05       TO   WK-KKDH10
               MOVE      HED-F042      TO   WK-KKDH11
               MOVE      HED-F29(1:10) TO   WK-KKDH12
               MOVE      HED-F202(1:7) TO   WK-KKDH13
               MOVE      HED-F22(1:10) TO   WK-KKDH14
               MOVE      SPACE         TO   WK-KKDH15
               MOVE    HED-F31(1:2)    TO   WK-KKDH16
               MOVE      HED-F041      TO   WK-KKDH17
               MOVE      HED-F042      TO   WK-KKDH18
               MOVE      SPACE         TO   WK-KKDH19
               MOVE      SPACE         TO   WK-KKDH20
               MOVE      SPACE         TO   WK-KKDH21
               MOVE      SPACE         TO   WK-KKDH22
               MOVE      SPACE         TO   WK-KKDH23
               MOVE      HED-F09       TO   WK-KKDH24
*#2019/03/11   EVALUATE  HED-F09
*#2019/03/11       WHEN  "00" MOVE "0" TO   WK-KKDH25
*#2019/03/11       WHEN  "01" MOVE "3" TO   WK-KKDH25
*#2019/03/11       WHEN  "02" MOVE "2" TO   WK-KKDH25
*#2019/03/11       WHEN  "03" MOVE "4" TO   WK-KKDH25
*#2019/03/11       WHEN  "05" MOVE "6" TO   WK-KKDH25
*#2019/03/11       WHEN  "51" MOVE "8" TO   WK-KKDH25
*#2019/03/11       WHEN  "52" MOVE "7" TO   WK-KKDH25
*#2019/03/11       WHEN  "71" MOVE "9" TO   WK-KKDH25
*#2019/03/11       WHEN  "81" MOVE "A" TO   WK-KKDH25
*#2019/03/11       WHEN  "82" MOVE "1" TO   WK-KKDH25
*#2019/03/11       WHEN  "83" MOVE "B" TO   WK-KKDH25
*#2019/03/11       WHEN OTHER MOVE "0" TO   WK-KKDH25
*#2019/03/11   END-EVALUATE
               MOVE      HED-F10       TO   WK-KKDH26
               MOVE      HED-F14       TO   WK-KKDH27
***************ケーヨー用
               MOVE      HED-F03       TO   WK-KKDH28
               MOVE      HED-F061      TO   WK-KKDH29
               MOVE      HED-F134      TO   WK-KKDH30
               MOVE      HED-F264      TO   WK-KKDH31
               MOVE      HED-F27       TO   WK-KKDH32
               MOVE      HED-F31       TO   WK-KKDH33
               MOVE      HED-F311      TO   WK-KKDH34
*****************************************************
***************店舗マスタ索引
               MOVE      SPACE         TO   TEN-REC
               INITIALIZE                   TEN-REC
               MOVE 1731               TO   TEN-F52
               MOVE WK-JOH-F21         TO   TEN-F011
               PERFORM HTENMS-READ-SEC
               IF   HTENMS-CHK-FLG = SPACE
***************#2019/03/11 NAV ST
                    MOVE  SPACE           TO   HSB-REC
                    INITIALIZE                 HSB-REC
                    MOVE  1731            TO   HSB-F01
                    MOVE  HED-F09         TO   HSB-F02
                    PERFORM DCMHSBF-READ-SEC
                    IF  DCMHSBF-INV-FLG  =  SPACE
                        MOVE  HSB-F03     TO   WK-KKDH25
                    ELSE
                        MOVE  "1"         TO   WK-KKDH25
                        DISPLAY NC"発注種別未登録エラー"
                            " " NC"取引先＝" HSB-F01
                            " " NC"発注種別区分＝" HSB-F02
                              NC"未登録の発注種別区分を確認！"
                    END-IF
***************#2019/03/11 NAV ED
                    MOVE   WK-KKDH-REC  TO   KEI1-REC
                    WRITE  KEI1-REC
                    ADD    1           TO   JCA-CNTKEYO1
                    MOVE   1731        TO   JOH-K03
               ELSE
***************#2019/03/11 NAV ST
                    MOVE  SPACE           TO   HSB-REC
                    INITIALIZE                 HSB-REC
                    MOVE  1732            TO   HSB-F01
                    MOVE  HED-F09         TO   HSB-F02
                    PERFORM DCMHSBF-READ-SEC
                    IF  DCMHSBF-INV-FLG  =  SPACE
                        MOVE  HSB-F03     TO   WK-KKDH25
                    ELSE
                        MOVE  "1"         TO   WK-KKDH25
                        DISPLAY NC"発注種別未登録エラー"
                            " " NC"取引先＝" HSB-F01
                            " " NC"発注種別区分＝" HSB-F02
                              NC"未登録の発注種別区分を確認！"
                    END-IF
***************#2019/03/11 NAV ED
                    MOVE   WK-KKDH-REC  TO   KEI2-REC
                    WRITE  KEI2-REC
                    ADD    1           TO   JCA-CNTKEYO2
                    MOVE   1732        TO   JOH-K03
               END-IF
***************ケーヨー追加（植物）
               WHEN  000760
               MOVE      SPACE         TO   WK-KKDH-REC
               INITIALIZE                   WK-KKDH-REC
               MOVE      "B"           TO   WK-KKDH01
               MOVE      SPACE         TO   WK-KKDH02
               MOVE      760           TO   WK-KKDH03
               MOVE      ZERO          TO   WK-KKDH04
               MOVE      HED-F21(2:3)  TO   WK-KKDH05
               MOVE      HED-F21       TO   WK-JOH-F21
               MOVE      JOH-K06(4:6)  TO   WK-KKDH06
               MOVE      SPACE         TO   WK-KKDH07
               MOVE      HED-F14(2:2)  TO   WK-KKDH08
               MOVE      ZERO          TO   WK-KKDH09
               MOVE      HED-F05       TO   WK-KKDH10
               MOVE      HED-F042      TO   WK-KKDH11
               MOVE      HED-F29(1:10) TO   WK-KKDH12
               MOVE      HED-F202(1:7) TO   WK-KKDH13
               MOVE      HED-F22(1:10) TO   WK-KKDH14
               MOVE      SPACE         TO   WK-KKDH15
               MOVE    HED-F31(1:2)    TO   WK-KKDH16
               MOVE      HED-F041      TO   WK-KKDH17
               MOVE      HED-F042      TO   WK-KKDH18
               MOVE      SPACE         TO   WK-KKDH19
               MOVE      SPACE         TO   WK-KKDH20
               MOVE      SPACE         TO   WK-KKDH21
               MOVE      SPACE         TO   WK-KKDH22
               MOVE      SPACE         TO   WK-KKDH23
               MOVE      HED-F09       TO   WK-KKDH24
               EVALUATE  HED-F09
                   WHEN  "00" MOVE "0" TO   WK-KKDH25
                   WHEN  "01" MOVE "3" TO   WK-KKDH25
                   WHEN  "02" MOVE "2" TO   WK-KKDH25
                   WHEN  "03" MOVE "4" TO   WK-KKDH25
                   WHEN  "05" MOVE "6" TO   WK-KKDH25
                   WHEN  "51" MOVE "8" TO   WK-KKDH25
                   WHEN  "52" MOVE "7" TO   WK-KKDH25
                   WHEN  "71" MOVE "9" TO   WK-KKDH25
                   WHEN  "81" MOVE "A" TO   WK-KKDH25
                   WHEN  "82" MOVE "1" TO   WK-KKDH25
                   WHEN  "83" MOVE "B" TO   WK-KKDH25
                   WHEN OTHER MOVE "0" TO   WK-KKDH25
               END-EVALUATE
               MOVE      HED-F10       TO   WK-KKDH26
               MOVE      HED-F14       TO   WK-KKDH27
***************ケーヨー用
               MOVE      HED-F03       TO   WK-KKDH28
               MOVE      HED-F061      TO   WK-KKDH29
               MOVE      HED-F134      TO   WK-KKDH30
               MOVE      HED-F264      TO   WK-KKDH31
               MOVE      HED-F27       TO   WK-KKDH32
               MOVE      HED-F31       TO   WK-KKDH33
               MOVE      HED-F311      TO   WK-KKDH34
*****************************************************
***************店舗マスタ索引
               MOVE      SPACE         TO   TEN-REC
               INITIALIZE                   TEN-REC
               MOVE 7601               TO   TEN-F52
               MOVE WK-JOH-F21         TO   TEN-F011
               PERFORM HTENMS-READ-SEC
               IF   HTENMS-CHK-FLG = SPACE
***************#2019/03/11 NAV ST
                    MOVE  SPACE           TO   HSB-REC
                    INITIALIZE                 HSB-REC
                    MOVE  7601            TO   HSB-F01
                    MOVE  HED-F09         TO   HSB-F02
                    PERFORM DCMHSBF-READ-SEC
                    IF  DCMHSBF-INV-FLG  =  SPACE
                        MOVE  HSB-F03     TO   WK-KKDH25
                    ELSE
                        MOVE  "1"         TO   WK-KKDH25
                        DISPLAY NC"発注種別未登録エラー"
                            " " NC"取引先＝" HSB-F01
                            " " NC"発注種別区分＝" HSB-F02
                              NC"未登録の発注種別区分を確認！"
                    END-IF
***************#2019/03/11 NAV ED
                    MOVE   WK-KKDH-REC  TO   KEI3-REC
                    WRITE  KEI3-REC
                    ADD    1           TO   JCA-CNTKEYO3
                    MOVE   7601        TO   JOH-K03
               ELSE
***************#2019/03/11 NAV ST
                    MOVE  SPACE           TO   HSB-REC
                    INITIALIZE                 HSB-REC
                    MOVE  7602            TO   HSB-F01
                    MOVE  HED-F09         TO   HSB-F02
                    PERFORM DCMHSBF-READ-SEC
                    IF  DCMHSBF-INV-FLG  =  SPACE
                        MOVE  HSB-F03     TO   WK-KKDH25
                    ELSE
                        MOVE  "1"         TO   WK-KKDH25
                        DISPLAY NC"発注種別未登録エラー"
                            " " NC"取引先＝" HSB-F01
                            " " NC"発注種別区分＝" HSB-F02
                              NC"未登録の発注種別区分を確認！"
                    END-IF
***************#2019/03/11 NAV ED
                    MOVE   WK-KKDH-REC  TO   KEI4-REC
                    WRITE  KEI4-REC
                    ADD    1           TO   JCA-CNTKEYO4
                    MOVE   7602        TO   JOH-K03
               END-IF
*#2018/02/13 NAV END  ｹｰﾖｰ##########################


***************対象外
               WHEN  OTHER
               MOVE      SPACE       TO   WK-DEPB-REC
               INITIALIZE                 WK-DEPB-REC
               MOVE      "H"         TO   WK-DEPB01
               MOVE      ZERO        TO   WK-DEPB02
***************MOVE      HED-F03     TO   WK-DEPB03
***************MOVE      JOH-F03(3:7) TO   WK-DEPB03
               MOVE      JOH-K06(3:7) TO   WK-DEPB03
               MOVE      HED-F05     TO   WK-DEPB04
               MOVE      HED-F06     TO   WK-DEPB05
***************MOVE      HED-F27     TO   WK-DEPB061
               MOVE      HED-F304    TO   WK-DEPB061
               MOVE      ZERO        TO   WK-DEPB062
               MOVE      HED-F29     TO   WK-DEPB07
               MOVE      HED-F21     TO   WK-DEPB081
               MOVE      SPACE       TO   WK-DEPB082
               MOVE      HED-F22     TO   WK-DEPB09
               MOVE      HED-F14     TO   WK-DEPB101
               MOVE      SPACE       TO   WK-DEPB102
               MOVE      ZERO        TO   WK-DEPB11
               MOVE      ZERO        TO   WK-DEPB12
               MOVE      ZERO        TO   WK-DEPB13
               MOVE      HED-F09     TO   WK-DEPB14
               MOVE      HED-F10     TO   WK-DEPB15
               MOVE      SPACE       TO   WK-DEPB16
               MOVE      SPACE       TO   WK-DEPB17
               MOVE      WK-DEPB-REC TO   JCA8-REC
               WRITE     JCA8-REC
               ADD       1           TO   JCA-CNT8
               MOVE      880         TO   JOH-K03

           END-EVALUATE

           PERFORM  TOKMS2-READ-SEC
           IF TOKMS2-INV-FLG = "INV"
              DISPLAY "##ﾄﾘﾋｷｻｷ ｿｳｺ ｼｭﾄｸｴﾗｰ##" UPON CONS
              STOP  RUN
           ELSE
              MOVE  TOK-F81 TO  WK-TOK-F81
           END-IF
           PERFORM   JHMRUTL1-READ-SEC
     END-IF.
*明細行
     IF    EDI-01  =  "DT"
***********取引先コードにより処理を振り分けます。
           EVALUATE  WK-JOH-F304
***************ホーマック（資材）
               WHEN  000880
               WHEN  000882
               WHEN  000883
***************ＪＣＡフォーマット初期化(明細）
               MOVE      SPACE       TO   WK-DEPD-REC
               INITIALIZE                 WK-DEPD-REC
***************ワークエリア初期化
               MOVE      SPACE       TO   MEI-REC
               INITIALIZE                 MEI-REC
***************明細情報→ワークにセット
               MOVE      EDI-REC     TO   MEI-REC
***************基本情報データ項目セット
               MOVE      MEI-F03     TO   JOH-K07
               MOVE      MEI-REC     TO   JOH-K21
***************欠品区分は０セット
               MOVE      ZERO        TO   JOH-M20
***************基本情報データ更新
               WRITE JOH-REC
               ADD       1           TO   JOH-CNT
***************ＪＣＡフォーマット項目セット
               MOVE      "L"         TO   WK-DEPD01
               MOVE      1           TO   WK-DEPD02
               MOVE      MEI-F04     TO   WK-DEPD031
               MOVE      SPACE       TO   WK-DEPD032
               MOVE      MEI-F07     TO   WK-DEPD04
***************MOVE      MEI-F10     TO   WK-DEPD05
               COMPUTE WK-DEPD05 = MEI-F10 / 100
               COMPUTE WK-DEPD06 = MEI-F15 / 100
               MOVE      MEI-F13     TO   WK-DEPD07
               MOVE      MEI-F16     TO   WK-DEPD08
               MOVE      MEI-F14     TO   WK-DEPD09
               MOVE      MEI-F02     TO   WK-DEPD10
               MOVE      SPACE       TO   WK-DEPD11
***************2014/08/19 NAV ST 発注納品方法変更対応
               MOVE      MEI-F09     TO   WK-DEPD111
               MOVE      MEI-F091    TO   WK-DEPD112
***************2014/08/19 NAV ED 発注納品方法変更対応
               EVALUATE  WK-JOH-F17
                   WHEN  2
                         MOVE   WK-DEPD-REC TO JCA1-REC
                         WRITE  JCA1-REC
                         ADD    1    TO   JCA-CNT1
                   WHEN  3
                         MOVE   WK-DEPD-REC TO JCA2-REC
                         WRITE  JCA2-REC
                         ADD    1    TO   JCA-CNT2
                   WHEN  4
                         MOVE   WK-DEPD-REC TO JCA3-REC
                         WRITE  JCA3-REC
                         ADD    1    TO   JCA-CNT3
                   WHEN  OTHER
                         MOVE   WK-DEPD-REC TO JCA8-REC
                         WRITE  JCA8-REC
                        ADD    1    TO   JCA-CNT8
               END-EVALUATE
***************ＪＣＡフォーマット初期化(トレイラー）
               MOVE      SPACE       TO   WK-DEPT-REC
               INITIALIZE                 WK-DEPT-REC
               MOVE      "T"         TO   WK-DEPT01
               MOVE      ZERO        TO   WK-DEPT02
               MOVE      MEI-F13     TO   WK-DEPT03
               MOVE      MEI-F14     TO   WK-DEPT04
               MOVE      SPACE       TO   WK-DEPT05
               EVALUATE  WK-JOH-F17
                   WHEN  2
                         MOVE   WK-DEPT-REC TO JCA1-REC
                         WRITE  JCA1-REC
                         ADD    1    TO   JCA-CNT1
                   WHEN  3
                         MOVE   WK-DEPT-REC TO JCA2-REC
                         WRITE  JCA2-REC
                         ADD    1    TO   JCA-CNT2
                   WHEN  4
                         MOVE   WK-DEPT-REC TO JCA3-REC
                         WRITE  JCA3-REC
                         ADD    1    TO   JCA-CNT3
                   WHEN  OTHER
                         MOVE   WK-DEPT-REC TO JCA8-REC
                         WRITE  JCA8-REC
                         ADD    1    TO   JCA-CNT8
               END-EVALUATE
***************ホーマック（植物）
               WHEN  001427
               WHEN  014272
               WHEN  014273
***************ＪＣＡフォーマット初期化(明細）
               MOVE      SPACE       TO   WK-DEPD-REC
               INITIALIZE                 WK-DEPD-REC
***************ワークエリア初期化
               MOVE      SPACE       TO   MEI-REC
               INITIALIZE                 MEI-REC
***************明細情報→ワークにセット
               MOVE      EDI-REC     TO   MEI-REC
***************基本情報データ項目セット
               MOVE      MEI-F03     TO   JOH-K07
               MOVE      MEI-REC     TO   JOH-K21
***************欠品区分は０セット
               MOVE      ZERO        TO   JOH-M20
***************基本情報データ更新
               WRITE JOH-REC
               ADD       1           TO   JOH-CNT
***************ＪＣＡフォーマット項目セット
               MOVE      "L"         TO   WK-DEPD01
               MOVE      1           TO   WK-DEPD02
               MOVE      MEI-F04     TO   WK-DEPD031
               MOVE      SPACE       TO   WK-DEPD032
               MOVE      MEI-F07     TO   WK-DEPD04
***************MOVE      MEI-F10     TO   WK-DEPD05
               COMPUTE WK-DEPD05 = MEI-F10 / 100
               COMPUTE WK-DEPD06 = MEI-F15 / 100
               MOVE      MEI-F13     TO   WK-DEPD07
               MOVE      MEI-F16     TO   WK-DEPD08
               MOVE      MEI-F14     TO   WK-DEPD09
               MOVE      MEI-F02     TO   WK-DEPD10
               MOVE      SPACE       TO   WK-DEPD11
***************2014/08/19 NAV ST 発注納品方法変更対応
               MOVE      MEI-F09     TO   WK-DEPD111
               MOVE      MEI-F091    TO   WK-DEPD112
***************2014/08/19 NAV ED 発注納品方法変更対応
               EVALUATE  WK-JOH-F17
                   WHEN  2
                         MOVE   WK-DEPD-REC TO JCA5-REC
                         WRITE  JCA5-REC
                         ADD    1    TO   JCA-CNT5
                   WHEN  3
                         MOVE   WK-DEPD-REC TO JCA6-REC
                         WRITE  JCA6-REC
                         ADD    1    TO   JCA-CNT6
                   WHEN  4
                         MOVE   WK-DEPD-REC TO JCA7-REC
                         WRITE  JCA7-REC
                         ADD    1    TO   JCA-CNT7
                   WHEN  OTHER
                         MOVE   WK-DEPD-REC TO JCA8-REC
                         WRITE  JCA8-REC
                        ADD    1    TO   JCA-CNT8
               END-EVALUATE
***************ＪＣＡフォーマット初期化(トレイラー）
               MOVE      SPACE       TO   WK-DEPT-REC
               INITIALIZE                 WK-DEPT-REC
               MOVE      "T"         TO   WK-DEPT01
               MOVE      ZERO        TO   WK-DEPT02
               MOVE      MEI-F13     TO   WK-DEPT03
               MOVE      MEI-F14     TO   WK-DEPT04
               MOVE      SPACE       TO   WK-DEPT05
               EVALUATE  WK-JOH-F17
                   WHEN  2
                         MOVE   WK-DEPT-REC TO JCA5-REC
                         WRITE  JCA5-REC
                         ADD    1    TO   JCA-CNT5
                   WHEN  3
                         MOVE   WK-DEPT-REC TO JCA6-REC
                         WRITE  JCA6-REC
                         ADD    1    TO   JCA-CNT6
                   WHEN  4
                         MOVE   WK-DEPT-REC TO JCA7-REC
                         WRITE  JCA7-REC
                         ADD    1    TO   JCA-CNT7
                   WHEN  OTHER
                         MOVE   WK-DEPT-REC TO JCA8-REC
                         WRITE  JCA8-REC
                         ADD    1    TO   JCA-CNT8
               END-EVALUATE

***************カーマ（資材）
               WHEN  013938
***************ＪＣＡフォーマット初期化(明細）
               MOVE      SPACE       TO   WK-KDD-REC
               INITIALIZE                 WK-KDD-REC
***************ワークエリア初期化
               MOVE      SPACE       TO   MEI-REC
               INITIALIZE                 MEI-REC
***************明細情報→ワークにセット
               MOVE      EDI-REC     TO   MEI-REC
***************基本情報データ項目セット
               MOVE      MEI-F03     TO   JOH-K07
               MOVE      MEI-REC     TO   JOH-K21
***************欠品区分は０セット
               MOVE      ZERO        TO   JOH-M20
               MOVE      JOH-K05(3:3) TO   WK-HEN-DENNO1
               MOVE      JOH-K06(4:6) TO   WK-HEN-DENNO2
               MOVE      WK-DENNO1   TO   WK-DENNO
               MOVE      WK-HEN-DENNO TO   JOH-K06
***************基本情報データ更新
               WRITE JOH-REC
               ADD       1           TO   JOH-CNT
***************ＪＣＡフォーマット項目セット
               MOVE      "D"         TO   WK-KDD01
               MOVE      ZERO        TO   WK-KDD02
               MOVE      HED-F21(2:3) TO  WK-KDD03
               MOVE      HED-F03(4:6) TO  WK-KDD04
               MOVE      MEI-F03(2:2) TO  WK-KDD05
               MOVE      MEI-F04(1:7) TO  WK-KDD06
               MOVE      ZERO        TO   WK-KDD07
               MOVE      MEI-F07(1:20) TO WK-KDD08
               MOVE      MEI-F084(1:20) TO  WK-KDD09
               MOVE      MEI-F02     TO   WK-KDD10
***************MOVE      MEI-F10(3:6) TO  WK-KDD11
               COMPUTE   WK-KDD11 = MEI-F10 / 100
***************MOVE      MEI-F15     TO   WK-KDD12
               COMPUTE   WK-KDD12 = MEI-F15 / 100
               MOVE      MEI-F16     TO   WK-KDD13
               MOVE      SPACE       TO   WK-KDD14
**************#2017/02/09 NAV ST 形状区分、発注単位区分
               MOVE      MEI-F091    TO   WK-KDD15
               MOVE      MEI-F09     TO   WK-KDD16
**************#2017/02/09 NAV ED
**************#2017/02/17 NAV ST
               MOVE      WK-JOH-F21  TO   WK-KDD17
***************MOVE      WK-KDD-REC  TO   KHM1-REC
*              WRITE     KHM1-REC
***************ADD       1           TO   JCA-CNTKAMA1
***************店舗マスタ索引
               MOVE      SPACE       TO   TEN-REC
               INITIALIZE                 TEN-REC
               MOVE 13938            TO   TEN-F52
               MOVE WK-JOH-F21       TO   TEN-F011
               PERFORM HTENMS-READ-SEC
               IF   HTENMS-CHK-FLG = SPACE
                    MOVE      WK-KDD-REC  TO   KHM1-REC
                    WRITE     KHM1-REC
                    ADD       1           TO   JCA-CNTKAMA1
               ELSE
                    MOVE      WK-KDD-REC  TO   KHM3-REC
                    WRITE     KHM3-REC
                    ADD       1           TO   JCA-CNTKAMA3
               END-IF
**************#2017/02/17 NAV ED

***************カーマ（植物）
               WHEN  017137
***************ＪＣＡフォーマット初期化(明細）
               MOVE      SPACE       TO   WK-KDD-REC
               INITIALIZE                 WK-KDD-REC
***************ワークエリア初期化
               MOVE      SPACE       TO   MEI-REC
               INITIALIZE                 MEI-REC
***************明細情報→ワークにセット
               MOVE      EDI-REC     TO   MEI-REC
***************基本情報データ項目セット
               MOVE      MEI-F03     TO   JOH-K07
               MOVE      MEI-REC     TO   JOH-K21
***************欠品区分は０セット
               MOVE      ZERO        TO   JOH-M20
               MOVE      JOH-K05(3:3) TO   WK-HEN-DENNO1
               MOVE      JOH-K06(4:6) TO   WK-HEN-DENNO2
               MOVE      WK-DENNO1   TO   WK-DENNO
               MOVE      WK-HEN-DENNO TO   JOH-K06
***************基本情報データ更新
               WRITE JOH-REC
               ADD       1           TO   JOH-CNT
***************ＪＣＡフォーマット項目セット
               MOVE      "D"         TO   WK-KDD01
               MOVE      ZERO        TO   WK-KDD02
               MOVE      HED-F21(2:3) TO  WK-KDD03
               MOVE      HED-F03(4:6) TO  WK-KDD04
               MOVE      MEI-F03(2:2) TO  WK-KDD05
               MOVE      MEI-F04(1:7) TO  WK-KDD06
               MOVE      ZERO        TO   WK-KDD07
               MOVE      MEI-F07(1:20) TO WK-KDD08
               MOVE      MEI-F084(1:20) TO  WK-KDD09
               MOVE      MEI-F02     TO   WK-KDD10
***************MOVE      MEI-F10(3:6) TO  WK-KDD11
               COMPUTE WK-KDD11 = MEI-F10 / 100
***************MOVE      MEI-F15     TO   WK-KDD12
               COMPUTE WK-KDD12 = MEI-F15 / 100
               MOVE      MEI-F16     TO   WK-KDD13
               MOVE      SPACE       TO   WK-KDD14
**************#2017/02/09 NAV ST 形状区分、発注単位区分
               MOVE      MEI-F091    TO   WK-KDD15
               MOVE      MEI-F09     TO   WK-KDD16
**************#2017/02/09 NAV ED
**************#2017/02/17 NAV ST
               MOVE      WK-JOH-F21  TO   WK-KDD17
***************MOVE      WK-KDD-REC  TO   KHM2-REC
*              WRITE     KHM2-REC
***************ADD       1           TO   JCA-CNTKAMA2
***************店舗マスタ索引
               MOVE      SPACE       TO   TEN-REC
               INITIALIZE                 TEN-REC
               MOVE 17137            TO   TEN-F52
               MOVE WK-JOH-F21       TO   TEN-F011
               PERFORM HTENMS-READ-SEC
               IF   HTENMS-CHK-FLG = SPACE
                    MOVE      WK-KDD-REC  TO   KHM2-REC
                    WRITE     KHM2-REC
                    ADD       1           TO   JCA-CNTKAMA2
               ELSE
                    MOVE      WK-KDD-REC  TO   KHM4-REC
                    WRITE     KHM4-REC
                    ADD       1           TO   JCA-CNTKAMA4
               END-IF
**************#2017/02/17 NAV ED

***************ダイキ
***************WHEN  100403
***************WHEN  100403  WHEN  100441
*********** ST 2009/07/30 取引先ＣＤ変更対応
***************WHEN  100403  WHEN  100441  WHEN  100434
               WHEN  100403  WHEN  100441
*********** ED 2009/07/30 取引先ＣＤ変更対応
***************ＪＣＡフォーマット初期化(明細）
               MOVE      SPACE       TO   WK-DDD-REC
               INITIALIZE                 WK-DDD-REC
***************ワークエリア初期化
               MOVE      SPACE       TO   MEI-REC
               INITIALIZE                 MEI-REC
***************明細情報→ワークにセット
               MOVE      EDI-REC     TO   MEI-REC
***************基本情報データ項目セット
               MOVE      MEI-F03     TO   JOH-K07
               MOVE      MEI-REC     TO   JOH-K21
***************欠品区分は０セット
               MOVE      ZERO        TO   JOH-M20
               MOVE      JOH-K05(3:3) TO   WK-HEN-DENNO1
               MOVE      JOH-K06(4:6) TO   WK-HEN-DENNO2
               MOVE      WK-DENNO1    TO   WK-DENNO
               MOVE      WK-HEN-DENNO TO   JOH-K06
***************基本情報データ更新
               WRITE JOH-REC
               ADD       1           TO   JOH-CNT
***************ＪＣＡフォーマット項目セット
               MOVE      "D"         TO   WK-DDD01
               MOVE      "01"        TO   WK-DDD02
               MOVE      MEI-F03(2:2) TO  WK-DDD03
               MOVE      MEI-F02     TO   WK-DDD04
               MOVE      MEI-F12(2:4) TO  WK-DDD05
               MOVE      MEI-F11(2:4) TO  WK-DDD06
               MOVE      SPACE       TO   WK-DDD07
***************MOVE      MEI-F10(3:6) TO  WK-DDD08
***************COMPUTE   WK-DDD08 = WK-DDD08 / 10
               COMPUTE   WK-DDD08 = MEI-F10  / 100
***************MOVE      MEI-F15     TO   WK-DDD09
               COMPUTE   WK-DDD09 = MEI-F15 / 100
               MOVE      MEI-F16     TO   WK-DDD10
               MOVE      MEI-F13     TO   WK-DDD11
               MOVE      MEI-F14     TO   WK-DDD12
               MOVE      ZERO        TO   WK-DDD131
               MOVE      ZERO        TO   WK-DDD132(1:1)
               MOVE      MEI-F04     TO   WK-DDD132(2:7)
               MOVE      MEI-F07(1:20) TO WK-DDD14
               MOVE      MEI-F084(1:20) TO WK-DDD15
               MOVE      MEI-F091    TO   WK-DDD16
               EVALUATE  MEI-F09
                   WHEN  "P"
                         MOVE      "ﾊﾞﾗ"  TO   WK-DDD17
                   WHEN  "C"
                         MOVE      "ｹｰｽ"  TO   WK-DDD17
                   WHEN  OTHER
                         MOVE      SPACE  TO   WK-DDD17
               END-EVALUATE
               MOVE      ZERO        TO   WK-DDD18
*#2017/06/06 NAV ST
***************MOVE      SPACE       TO   WK-DDD19
               MOVE      MEI-F09     TO   WK-DDD19
               MOVE      SPACE       TO   WK-DDD20
*#2017/06/06 NAV ED
*##            MOVE      WK-DDD-REC  TO   DAI1-REC
*##            WRITE     DAI1-REC
*##            ADD       1           TO   JCA-CNTDAIK1
***************店舗マスタ索引
               MOVE      SPACE       TO   TEN-REC
               INITIALIZE                 TEN-REC
               MOVE WK-HEN-TORICD    TO   TEN-F52
               MOVE WK-DDH05         TO   TEN-F011
               PERFORM HTENMS-READ-SEC
               IF   HTENMS-CHK-FLG = SPACE
                    MOVE      WK-DDD-REC  TO   DAI1-REC
                    WRITE     DAI1-REC
                    ADD       1           TO   JCA-CNTDAIK1
               ELSE
                    MOVE      WK-DDD-REC  TO   DAI3-REC
                    WRITE     DAI3-REC
                    ADD       1           TO   JCA-CNTDAIK3
               END-IF
***************ダイキ
***************WHEN  100403
               WHEN  100427
***************ＪＣＡフォーマット初期化(明細）
               MOVE      SPACE       TO   WK-DDD-REC
               INITIALIZE                 WK-DDD-REC
***************ワークエリア初期化
               MOVE      SPACE       TO   MEI-REC
               INITIALIZE                 MEI-REC
***************明細情報→ワークにセット
               MOVE      EDI-REC     TO   MEI-REC
***************基本情報データ項目セット
               MOVE      MEI-F03     TO   JOH-K07
               MOVE      MEI-REC     TO   JOH-K21
***************欠品区分は０セット
               MOVE      ZERO        TO   JOH-M20
               MOVE      JOH-K05(3:3) TO   WK-HEN-DENNO1
               MOVE      JOH-K06(4:6) TO   WK-HEN-DENNO2
               MOVE      WK-DENNO1    TO   WK-DENNO
               MOVE      WK-HEN-DENNO TO   JOH-K06
***************基本情報データ更新
               WRITE JOH-REC
               ADD       1           TO   JOH-CNT
***************ＪＣＡフォーマット項目セット
               MOVE      "D"         TO   WK-DDD01
               MOVE      "01"        TO   WK-DDD02
               MOVE      MEI-F03(2:2) TO  WK-DDD03
               MOVE      MEI-F02     TO   WK-DDD04
               MOVE      MEI-F12(2:4) TO  WK-DDD05
               MOVE      MEI-F11(2:4) TO  WK-DDD06
               MOVE      SPACE       TO   WK-DDD07
***************MOVE      MEI-F10(3:6) TO  WK-DDD08
***************COMPUTE   WK-DDD08 = WK-DDD08 / 10
               COMPUTE   WK-DDD08 = MEI-F10  / 100
***************MOVE      MEI-F15     TO   WK-DDD09
               COMPUTE   WK-DDD09 = MEI-F15 / 100
               MOVE      MEI-F16     TO   WK-DDD10
               MOVE      MEI-F13     TO   WK-DDD11
               MOVE      MEI-F14     TO   WK-DDD12
               MOVE      ZERO        TO   WK-DDD131
               MOVE      ZERO        TO   WK-DDD132(1:1)
               MOVE      MEI-F04     TO   WK-DDD132(2:7)
               MOVE      MEI-F07(1:20) TO WK-DDD14
               MOVE      MEI-F084(1:20) TO WK-DDD15
               MOVE      MEI-F091    TO   WK-DDD16
               EVALUATE  MEI-F09
                   WHEN  "P"
                         MOVE      "ﾊﾞﾗ"  TO   WK-DDD17
                   WHEN  "C"
                         MOVE      "ｹｰｽ"  TO   WK-DDD17
                   WHEN  OTHER
                         MOVE      SPACE  TO   WK-DDD17
               END-EVALUATE
               MOVE      ZERO        TO   WK-DDD18
*#2017/06/06 NAV ST
***************MOVE      SPACE       TO   WK-DDD19
               MOVE      MEI-F09     TO   WK-DDD19
               MOVE      SPACE       TO   WK-DDD20
*#2017/06/06 NAV ED
*##            MOVE      WK-DDD-REC  TO   DAI2-REC
*##            WRITE     DAI2-REC
*##            ADD       1           TO   JCA-CNTDAIK2
***************店舗マスタ索引
               MOVE      SPACE       TO   TEN-REC
               INITIALIZE                 TEN-REC
               MOVE WK-HEN-TORICD    TO   TEN-F52
               MOVE WK-DDH05         TO   TEN-F011
               PERFORM HTENMS-READ-SEC
               IF   HTENMS-CHK-FLG = SPACE
                    MOVE      WK-DDD-REC  TO   DAI2-REC
                    WRITE     DAI2-REC
                    ADD       1           TO   JCA-CNTDAIK2
               ELSE
                    MOVE      WK-DDD-REC  TO   DAI4-REC
                    WRITE     DAI4-REC
                    ADD       1           TO   JCA-CNTDAIK4
               END-IF

*#2018/03/13 NAV ST ｹｰﾖｰ#######################
***************ケーヨー追加（資材）
               WHEN  000173
***************ＪＣＡフォーマット初期化(明細）
               MOVE      SPACE          TO  WK-KKDD-REC
               INITIALIZE                   WK-KKDD-REC
***************ワークエリア初期化
               MOVE      SPACE          TO  MEI-REC
               INITIALIZE                   MEI-REC
***************明細情報→ワークにセット
               MOVE      EDI-REC        TO  MEI-REC
***************基本情報データ項目セット
               MOVE      MEI-F03        TO  JOH-K07
               MOVE      MEI-REC        TO  JOH-K21
***************欠品区分は０セット
               MOVE      ZERO           TO  JOH-M20
***************基本情報データ更新
               WRITE JOH-REC
               ADD       1              TO  JOH-CNT
***************ＪＣＡフォーマット項目セット
               MOVE      "D"            TO  WK-KKDD01
               MOVE      ZERO           TO  WK-KKDD02
               MOVE      HED-F21(2:3)   TO  WK-KKDD03
               MOVE      HED-F03(4:6)   TO  WK-KKDD04
               MOVE      MEI-F03(2:2)   TO  WK-KKDD05
               MOVE      MEI-F04(1:7)   TO  WK-KKDD06
               MOVE      ZERO           TO  WK-KKDD07
               MOVE      MEI-F07(1:20)  TO  WK-KKDD08
               MOVE      MEI-F084(1:20) TO  WK-KKDD09
               MOVE      MEI-F02        TO  WK-KKDD10
               COMPUTE   WK-KKDD11 = MEI-F10 / 100
               COMPUTE   WK-KKDD12 = MEI-F15 / 100
               MOVE      MEI-F16        TO  WK-KKDD13
               MOVE      SPACE          TO  WK-KKDD14
**************#2017/02/09 NAV ST 形状区分、発注単位区分
               MOVE      MEI-F091       TO  WK-KKDD15
               MOVE      MEI-F09        TO  WK-KKDD16
               MOVE      WK-JOH-F21     TO  WK-KKDD17
***************店舗マスタ索引
               MOVE      SPACE          TO  TEN-REC
               INITIALIZE                   TEN-REC
               MOVE 1731                TO  TEN-F52
               MOVE WK-JOH-F21          TO  TEN-F011
               PERFORM HTENMS-READ-SEC
               IF   HTENMS-CHK-FLG = SPACE
                    MOVE   WK-KKDD-REC   TO  KEI1-REC
                    WRITE  KEI1-REC
                    ADD    1            TO  JCA-CNTKEYO1
               ELSE
                    MOVE   WK-KKDD-REC   TO  KEI2-REC
                    WRITE  KEI2-REC
                    ADD    1            TO  JCA-CNTKEYO2
               END-IF
***************ケーヨー追加（植物）
               WHEN  000760
***************ＪＣＡフォーマット初期化(明細）
               MOVE      SPACE          TO  WK-KKDD-REC
               INITIALIZE                   WK-KKDD-REC
***************ワークエリア初期化
               MOVE      SPACE          TO  MEI-REC
               INITIALIZE                   MEI-REC
***************明細情報→ワークにセット
               MOVE      EDI-REC        TO  MEI-REC
***************基本情報データ項目セット
               MOVE      MEI-F03        TO  JOH-K07
               MOVE      MEI-REC        TO  JOH-K21
***************欠品区分は０セット
               MOVE      ZERO           TO  JOH-M20
***************基本情報データ更新
               WRITE JOH-REC
               ADD       1              TO  JOH-CNT
***************ＪＣＡフォーマット項目セット
               MOVE      "D"            TO  WK-KKDD01
               MOVE      ZERO           TO  WK-KKDD02
               MOVE      HED-F21(2:3)   TO  WK-KKDD03
               MOVE      HED-F03(4:6)   TO  WK-KKDD04
               MOVE      MEI-F03(2:2)   TO  WK-KKDD05
               MOVE      MEI-F04(1:7)   TO  WK-KKDD06
               MOVE      ZERO           TO  WK-KKDD07
               MOVE      MEI-F07(1:20)  TO  WK-KKDD08
               MOVE      MEI-F084(1:20) TO  WK-KKDD09
               MOVE      MEI-F02        TO  WK-KKDD10
               COMPUTE WK-KKDD11 = MEI-F10 / 100
               COMPUTE WK-KKDD12 = MEI-F15 / 100
               MOVE      MEI-F16        TO  WK-KKDD13
               MOVE      SPACE          TO  WK-KKDD14
**************#2017/02/09 NAV ST 形状区分、発注単位区分
               MOVE      MEI-F091       TO  WK-KKDD15
               MOVE      MEI-F09        TO  WK-KKDD16
               MOVE      WK-JOH-F21     TO  WK-KKDD17
***************店舗マスタ索引
               MOVE      SPACE         TO  TEN-REC
               INITIALIZE                  TEN-REC
               MOVE 7601               TO  TEN-F52
               MOVE WK-JOH-F21         TO  TEN-F011
               PERFORM HTENMS-READ-SEC
               IF   HTENMS-CHK-FLG = SPACE
                    MOVE   WK-KKDD-REC  TO  KEI3-REC
                    WRITE  KEI3-REC
                    ADD    1           TO  JCA-CNTKEYO3
               ELSE
                    MOVE   WK-KKDD-REC  TO  KEI4-REC
                    WRITE  KEI4-REC
                    ADD    1           TO  JCA-CNTKEYO4
               END-IF
*#2018/03/13 NAV ED ｹｰﾖｰ#######################

***************対象外
               WHEN  OTHER
***************ＪＣＡフォーマット初期化(明細）
               MOVE      SPACE       TO   WK-DEPD-REC
               INITIALIZE                 WK-DEPD-REC
***************ワークエリア初期化
               MOVE      SPACE       TO   MEI-REC
               INITIALIZE                 MEI-REC
***************明細情報→ワークにセット
               MOVE      EDI-REC     TO   MEI-REC
***************基本情報データ項目セット
               MOVE      MEI-F03     TO   JOH-K07
               MOVE      MEI-REC     TO   JOH-K21
***************欠品区分は０セット
               MOVE      ZERO        TO   JOH-M20
***************基本情報データ更新
               WRITE JOH-REC
               ADD       1           TO   JOH-CNT
***************ＪＣＡフォーマット項目セット
               MOVE      "L"         TO   WK-DEPD01
               MOVE      1           TO   WK-DEPD02
               MOVE      MEI-F04     TO   WK-DEPD031
               MOVE      SPACE       TO   WK-DEPD032
               MOVE      MEI-F07     TO   WK-DEPD04
***************MOVE      MEI-F10     TO   WK-DEPD05
               COMPUTE WK-DEPD05 = MEI-F10 / 100
               COMPUTE WK-DEPD06 = MEI-F15 / 100
               MOVE      MEI-F13     TO   WK-DEPD07
               MOVE      MEI-F16     TO   WK-DEPD08
               MOVE      MEI-F14     TO   WK-DEPD09
               MOVE      MEI-F02     TO   WK-DEPD10
               MOVE      SPACE       TO   WK-DEPD11
               MOVE      WK-DEPD-REC TO   JCA8-REC
               WRITE     JCA8-REC
               ADD       1           TO   JCA-CNT8

           END-EVALUATE

     END-IF.
*
     PERFORM JEDICOS-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ファイル読込　　　　　　　　　　　　　　　　　　*
****************************************************************
 JEDICOS-READ-SEC      SECTION.
*
     MOVE "JEDICOS-READ-SEC" TO       S-NAME.
*
     READ     JEDICOS
              AT END
              MOVE     9      TO    END-FG
              NOT AT END
              ADD      1      TO    RD-CNT
     END-READ.
*
     IF   RD-CNT(6:3) = "000" OR "500"
          DISPLAY "READ-CNT = " RD-CNT   UPON CONS
     END-IF.
*
 JEDICOS-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　取引先マスタ読込　　　　　　　　　　　　　　　　*
****************************************************************
 TOKMS2-READ-SEC  SECTION.
*
     MOVE    SPACE         TO    TOK-REC
     INITIALIZE                  TOK-REC
     MOVE    JOH-K03       TO    TOK-F01
     READ    TOKMS2
             INVALID
             MOVE  "INV"   TO    TOKMS2-INV-FLG
             NOT  INVALID
             MOVE  SPACE   TO    TOKMS2-INV-FLG
     END-READ.
*
 TOKMS2-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　店舗マスタ読込　　　　　　　　　　　　　　　　　*
****************************************************************
 HTENMS-READ-SEC  SECTION.
*
     MOVE    SPACE         TO    HTENMS-CHK-FLG.
     READ    TENMS1
             INVALID
             MOVE  "INV"   TO    HTENMS-INV-FLG
             NOT  INVALID
             MOVE  SPACE   TO    HTENMS-INV-FLG
     END-READ.
*
     IF  HTENMS-INV-FLG = "INV"
         MOVE  SPACE       TO    HTENMS-CHK-FLG
     ELSE
         IF  TEN-F73 = SPACE
*************本社　ダイキ
             MOVE SPACE    TO    HTENMS-CHK-FLG
         ELSE
*************九州　ダイキ（サンコー）
             MOVE "CHK"    TO    HTENMS-CHK-FLG
         END-IF
     END-IF.
*
 HTENMS-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ルートマスタ読込　　　　　　　　　　　　　　　　*
****************************************************************
 JHMRUTL1-READ-SEC  SECTION.
*
     MOVE     JOH-K03      TO        RUT-F01.
     MOVE     SPACE        TO        RUT-F02.
     MOVE     HED-F31(1:2) TO        RUT-F03.
*#2018/03/01 NAV ST ケーヨーの場合
     IF  JOH-K03  =  1731  OR  1732  OR  7601  OR  7602
         MOVE WK-KKDH27    TO        RUT-F02
*********DISPLAY "RUT-F01 = " RUT-F01 UPON CONS
*********DISPLAY "RUT-F02 = " RUT-F02 UPON CONS
*********DISPLAY "RUT-F03 = " RUT-F03 UPON CONS
     END-IF.
*#2018/03/01 NAV ED ケーヨーの場合
     READ     JHMRUTL1
         INVALID
           MOVE  "INV"     TO        JHMRUTL1-INV-FLG
           MOVE WK-TOK-F81 TO        JOH-K04
         NOT INVALID
           MOVE  RUT-F05   TO        JOH-K04
     END-READ.
*
 JHMRUTL1-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　発注種別変換マスタ読込　　　　　　　　　　　　　*
****************************************************************
 DCMHSBF-READ-SEC SECTION.
*
     READ    DCMHSBF
             INVALID
             MOVE  "INV"   TO    DCMHSBF-INV-FLG
             NOT  INVALID
             MOVE  SPACE   TO    DCMHSBF-INV-FLG
     END-READ.
*
 DCMHSBF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     CLOSE     JEDICOS  DJJOHOF  JCAFILE1  JCAFILE2 JCAFILE3.
     CLOSE     JCAKAMA1 JCAFILE5 JCAFILE6  JCAFILE7.
     CLOSE     JCAKAMA2 JCADAIK1 JCADAIK2 JCAFILE8.
     CLOSE     TENMS1   JCADAIK3 JCADAIK4.
*#2017/02/17 NAV ST
     CLOSE     JCAKAMA3  JCAKAMA4.
*#2017/02/17 NAV ED
*#2018/02/13 NAV ST
     CLOSE     JCAKEYO1  JCAKEYO2  JCAKEYO3  JCAKEYO4.
*#2018/02/13 NAV ED
*#2019/03/11 NAV ST
     CLOSE     DCMHSBF.
*#2019/03/11 NAV ED

*
     DISPLAY NC"＃基本情報データ　ＣＮＴ＝"  JOH-CNT
                                             UPON CONS.
     DISPLAY NC"＃ＨＣ資材　北海道ＣＮＴ＝"  JCA-CNT1
                                             UPON CONS.
     DISPLAY NC"＃ＨＣ資材　東北　ＣＮＴ＝"  JCA-CNT2
                                             UPON CONS.
     DISPLAY NC"＃ＨＣ資材　関東　ＣＮＴ＝"  JCA-CNT3
                                             UPON CONS.
     DISPLAY NC"＃ＨＣ植物　北海道ＣＮＴ＝"  JCA-CNT5
                                             UPON CONS.
     DISPLAY NC"＃ＨＣ植物　東北　ＣＮＴ＝"  JCA-CNT6
                                             UPON CONS.
     DISPLAY NC"＃ＨＣ植物　関東　ＣＮＴ＝"  JCA-CNT7
                                             UPON CONS.
     DISPLAY NC"＃カーマ資材　　　ＣＮＴ＝"  JCA-CNTKAMA1
                                             UPON CONS.
     DISPLAY NC"＃カーマ植物　　　ＣＮＴ＝"  JCA-CNTKAMA2
                                             UPON CONS.
*#2017/02/17 NAV ST
     DISPLAY NC"＃カーマ資材２　　ＣＮＴ＝"  JCA-CNTKAMA3
                                             UPON CONS.
     DISPLAY NC"＃カーマ植物２　　ＣＮＴ＝"  JCA-CNTKAMA4
                                             UPON CONS.
*#2017/02/17 NAV ED
     DISPLAY NC"＃ダイキ資材　　　ＣＮＴ＝"  JCA-CNTDAIK1
                                             UPON CONS.
     DISPLAY NC"＃ダイキ植物　　　ＣＮＴ＝"  JCA-CNTDAIK2
                                             UPON CONS.
     DISPLAY NC"＃ダイキ資材九州　ＣＮＴ＝"  JCA-CNTDAIK3
                                             UPON CONS.
     DISPLAY NC"＃ダイキ植物九州　ＣＮＴ＝"  JCA-CNTDAIK4
                                             UPON CONS.
*＃2018/02/13 NAV ST
     DISPLAY NC"＃ケーヨー資材東　ＣＮＴ＝"  JCA-CNTKEYO1
                                             UPON CONS.
     DISPLAY NC"＃ケーヨー資材西　ＣＮＴ＝"  JCA-CNTKEYO2
                                             UPON CONS.
     DISPLAY NC"＃ケーヨー植物東　ＣＮＴ＝"  JCA-CNTKEYO3
                                             UPON CONS.
     DISPLAY NC"＃ケーヨー植物東　ＣＮＴ＝"  JCA-CNTKEYO4
                                             UPON CONS.
*＃2018/02/13 NAV ED
     DISPLAY NC"＃対象外　　　　　ＣＮＴ＝"  JCA-CNT8
                                             UPON CONS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
