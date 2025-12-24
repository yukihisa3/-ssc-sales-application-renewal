# SJH8730B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SJH8730B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　ＤＣＭＪＡＰＡＮ　ＷｅｂＥＤＩ　　*
*    モジュール名　　　　：　ＪＥＤＩＣＯＳデータ変換　　　　　*
*    作成日／更新日　　　：　2007/05/17                        *
*    作成者／更新者　　　：　松野　　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　ＪＥＤＩＣＯＳにて受信したデータ　*
*                            をＪＣＡフォーマットに変換する。　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SJH8730B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          07/05/17.
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
*ＪＣＡフォーマット（ホーマック資材　北海道分）
     SELECT   JCAFILEA   ASSIGN    TO        DA-01-S-JCAFILEA
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    JCAA-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ＪＣＡフォーマット（ホーマック資材　仙台分）
     SELECT   JCAFILEB   ASSIGN    TO        DA-01-S-JCAFILEB
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    JCAB-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ＪＣＡフォーマット（ホーマック資材　関東分）
     SELECT   JCAFILEC   ASSIGN    TO        DA-01-S-JCAFILEC
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    JCAC-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ＪＣＡフォーマット（カーマ資材）
     SELECT   JCAKAMAA   ASSIGN    TO        DA-01-S-JCAKAMAA
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    KHMA-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ＪＣＡフォーマット（ホーマック植物　北海道分）
     SELECT   JCAFILEE   ASSIGN    TO        DA-01-S-JCAFILEE
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    JCAE-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ＪＣＡフォーマット（ホーマック植物　仙台分）
     SELECT   JCAFILEF   ASSIGN    TO        DA-01-S-JCAFILEF
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    JCAF-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ＪＣＡフォーマット（ホーマック植物　関東分）
     SELECT   JCAFILEG   ASSIGN    TO        DA-01-S-JCAFILEG
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    JCAG-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ＪＣＡフォーマット（カーマ植物）
     SELECT   JCAKAMAB   ASSIGN    TO        DA-01-S-JCAKAMAB
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    KHMB-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ＪＣＡフォーマット（ダイキ資材）
     SELECT   JCADAIK1   ASSIGN    TO        DA-01-S-JCADAIK1
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    DAI1-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ＪＣＡフォーマット（ダイキ植物）
     SELECT   JCADAIK2   ASSIGN    TO        DA-01-S-JCADAIK2
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    DAI2-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ＪＣＡフォーマット（対象外）
     SELECT   JCAFILEH   ASSIGN    TO        DA-01-S-JCAFILEH
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    JCAH-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    受信データ　ＲＬ＝　３１４９  ＢＦ＝　１
******************************************************************
 FD  JEDICOS
                        BLOCK CONTAINS      1    RECORDS
                        LABEL RECORD   IS   STANDARD.
*
 01  EDI-REC.
     03  EDI-01                  PIC  X(02).
     03  EDI-02                  PIC  X(3147).
******************************************************************
*    ＪＣＡフォーマットファイル（ホーマック資材　北海道）
******************************************************************
 FD  JCAFILEA            LABEL RECORD   IS   STANDARD.
*
 01  JCAA-REC.
     03  FILLER                   PIC X(128).
******************************************************************
*    ＪＣＡフォーマットファイル（ホーマック資材　東北）
******************************************************************
 FD  JCAFILEB            LABEL RECORD   IS   STANDARD.
*
 01  JCAB-REC.
     03  FILLER                   PIC X(128).
******************************************************************
*    ＪＣＡフォーマットファイル（ホーマック資材　関東）
******************************************************************
 FD  JCAFILEC            LABEL RECORD   IS   STANDARD.
*
 01  JCAC-REC.
     03  FILLER                   PIC X(128).
******************************************************************
*    ＪＣＡフォーマットファイル（カーマ資材）
******************************************************************
 FD  JCAKAMAA            LABEL RECORD   IS   STANDARD.
*
 01  KHMA-REC.
     03  FILLER                   PIC X(128).
******************************************************************
*    ＪＣＡフォーマットファイル（ホーマック植物　北海道）
******************************************************************
 FD  JCAFILEE            LABEL RECORD   IS   STANDARD.
*
 01  JCAE-REC.
     03  FILLER                   PIC X(128).
******************************************************************
*    ＪＣＡフォーマットファイル（ホーマック植物　東北）
******************************************************************
 FD  JCAFILEF            LABEL RECORD   IS   STANDARD.
*
 01  JCAF-REC.
     03  FILLER                   PIC X(128).
******************************************************************
*    ＪＣＡフォーマットファイル（ホーマック植物　関東）
******************************************************************
 FD  JCAFILEG            LABEL RECORD   IS   STANDARD.
*
 01  JCAG-REC.
     03  FILLER                   PIC X(128).
******************************************************************
*    ＪＣＡフォーマットファイル（カーマ植物）
******************************************************************
 FD  JCAKAMAB            LABEL RECORD   IS   STANDARD.
*
 01  KHMB-REC.
     03  FILLER                   PIC X(128).
******************************************************************
*    ＪＣＡフォーマットファイル（ダイキ資材）
******************************************************************
 FD  JCADAIK1            LABEL RECORD   IS   STANDARD.
*
 01  DAI1-REC.
     03  FILLER                   PIC X(128).
******************************************************************
*    ＪＣＡフォーマットファイル（ダイキ植物）
******************************************************************
 FD  JCADAIK2            LABEL RECORD   IS   STANDARD.
*
 01  DAI2-REC.
     03  FILLER                   PIC X(128).
******************************************************************
*    ＪＣＡフォーマットファイル（対象外）
******************************************************************
 FD  JCAFILEH            LABEL RECORD   IS   STANDARD.
*
 01  JCAH-REC.
     03  FILLER                   PIC X(128).
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*ヘッダ情報格納領域
     COPY   DJJYRHED OF XFDLIB  JOINING   HED  AS   PREFIX.
*ヘッダ情報格納領域
     COPY   DJJYRMEI OF XFDLIB  JOINING   MEI  AS   PREFIX.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  IDX                     PIC  9(02)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  JCA-CNTA                PIC  9(08)     VALUE  ZERO.
 01  JCA-CNTB                PIC  9(08)     VALUE  ZERO.
 01  JCA-CNTC                PIC  9(08)     VALUE  ZERO.
 01  JCA-CNTKAMAA            PIC  9(08)     VALUE  ZERO.
 01  JCA-CNTE                PIC  9(08)     VALUE  ZERO.
 01  JCA-CNTF                PIC  9(08)     VALUE  ZERO.
 01  JCA-CNTG                PIC  9(08)     VALUE  ZERO.
 01  JCA-CNTKAMAB            PIC  9(08)     VALUE  ZERO.
 01  JCA-CNTDAIK1            PIC  9(08)     VALUE  ZERO.
 01  JCA-CNTDAIK2            PIC  9(08)     VALUE  ZERO.
 01  JCA-CNTH                PIC  9(08)     VALUE  ZERO.
 01  TOKMS2-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  JHMRUTL1-INV-FLG        PIC  X(03)     VALUE  SPACE.
 01  WK-TOK-F81              PIC  X(02)     VALUE  SPACE.
 01  WK-HED-F17              PIC  9(02)     VALUE  ZERO.
 01  WK-HED-F304             PIC  X(06)     VALUE  SPACE.
*
*ヘッドレコード退避ワーク
 01  WK-DEPB-REC.
     03  WK-DEPB01          PIC  X(01).
     03  WK-DEPB02          PIC  9(01).
     03  WK-DEPB03          PIC  X(07).
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
     03  WK-DEPB16          PIC  X(14).
     03  WK-DEPB161         PIC  9(09).
     03  WK-DEPB17          PIC  X(01).

*    明細レコード退避ワーク
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
     03  WK-DEPD051         PIC  9(05)V9.
     03  WK-DEPD06          PIC  9(06)V99.
     03  WK-DEPD07          PIC  9(08).
     03  WK-DEPD08          PIC  9(06).
     03  WK-DEPD09          PIC  9(08).
     03  WK-DEPD10          PIC  X(13).
     03  WK-DEPD11          PIC  X(23).
*    合計レコード退避ワーク
 01  WK-DEPT-REC.
     03  WK-DEPT01          PIC  X(01).
     03  WK-DEPT02          PIC  9(01).
     03  WK-DEPT03          PIC  9(08).
     03  WK-DEPT04          PIC  9(08).
     03  WK-DEPT05          PIC  X(110).
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  EDI-STATUS        PIC  X(02).
     03  JCAA-STATUS       PIC  X(02).
     03  JCAB-STATUS       PIC  X(02).
     03  JCAC-STATUS       PIC  X(02).
     03  KHMA-STATUS       PIC  X(02).
     03  JCAE-STATUS       PIC  X(02).
     03  JCAF-STATUS       PIC  X(02).
     03  JCAG-STATUS       PIC  X(02).
     03  KHMB-STATUS       PIC  X(02).
     03  DAI1-STATUS       PIC  X(02).
     03  DAI2-STATUS       PIC  X(02).
     03  JCAH-STATUS       PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SJH8730B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJH8730B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJH8730B".
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
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
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
                        PROCEDURE   JCAFILEA.
     MOVE      "JCAFILEA"    TO   AB-FILE.
     MOVE      JCAA-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JCAFILEB.
     MOVE      "JCAFILEB"    TO   AB-FILE.
     MOVE      JCAB-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JCAFILEC.
     MOVE      "JCAFILEC"    TO   AB-FILE.
     MOVE      JCAC-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JCAKAMAA.
     MOVE      "JCAKAMAA"    TO   AB-FILE.
     MOVE      KHMA-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC6           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JCAFILEE.
     MOVE      "JCAFILEE"    TO   AB-FILE.
     MOVE      JCAE-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC7           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JCAFILEF.
     MOVE      "JCAFILEF"    TO   AB-FILE.
     MOVE      JCAF-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC8           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JCAFILEG.
     MOVE      "JCAFILEG"    TO   AB-FILE.
     MOVE      JCAG-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC9           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JCAKAMAB.
     MOVE      "JCAKAMAB"    TO   AB-FILE.
     MOVE      KHMB-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC10          SECTION.
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
 FILEERR-SEC11          SECTION.
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
 FILEERR-SEC12          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JCAFILEH.
     MOVE      "JCAFILEH"    TO   AB-FILE.
     MOVE      JCAH-STATUS   TO   AB-STS.
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
     OPEN     INPUT     JEDICOS.
     OPEN     OUTPUT    JCAFILEA  JCAFILEB  JCAFILEC  JCAKAMAA.
     OPEN     OUTPUT    JCAFILEE  JCAFILEF  JCAFILEG  JCAKAMAB.
     OPEN     OUTPUT    JCADAIK1  JCADAIK2  JCAFILEH.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT.
     MOVE     ZERO      TO        JCA-CNTA JCA-CNTB
                                  JCA-CNTC JCA-CNTKAMAA
                                  JCA-CNTE JCA-CNTF JCA-CNTG
                                  JCA-CNTKAMAB JCA-CNTDAIK1
                                  JCA-CNTDAIK1 JCA-CNTH.
     MOVE     SPACE     TO        WK-DEPB-REC.
     INITIALIZE                   WK-DEPB-REC.
     MOVE     SPACE     TO        WK-DEPD-REC.
     INITIALIZE                   WK-DEPD-REC.
     MOVE     SPACE     TO        WK-DEPT-REC.
     INITIALIZE                   WK-DEPT-REC.
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
***********JCA FORMAT 初期化
           MOVE      SPACE       TO   WK-DEPB-REC
           INITIALIZE                 WK-DEPB-REC
           MOVE      "H"         TO   WK-DEPB01
           MOVE      ZERO        TO   WK-DEPB02
           MOVE      HED-F03     TO   WK-DEPB03  WK-DEPB161
           MOVE      HED-F05     TO   WK-DEPB04
           MOVE      HED-F06     TO   WK-DEPB05
           MOVE      HED-F27     TO   WK-DEPB061
           MOVE      ZERO        TO   WK-DEPB062
           MOVE      HED-F29     TO   WK-DEPB07
           MOVE      HED-F21     TO   WK-DEPB081
           MOVE      SPACE       TO   WK-DEPB082
           MOVE      HED-F22     TO   WK-DEPB09
           MOVE      HED-F14     TO   WK-DEPB101
           MOVE      SPACE       TO   WK-DEPB102
***********MOVE      ZERO        TO   WK-DEPB11
           MOVE      HED-F081    TO   WK-DEPB11
           MOVE      ZERO        TO   WK-DEPB12
           MOVE      ZERO        TO   WK-DEPB13
           MOVE      ZERO        TO   WK-DEPB14
           MOVE      HED-F10     TO   WK-DEPB15
           MOVE      SPACE       TO   WK-DEPB16
           MOVE      SPACE       TO   WK-DEPB17
           MOVE      HED-F17     TO   WK-HED-F17
           MOVE      HED-F304    TO   WK-HED-F304
***********取引先別に処理を振り分けます。
           EVALUATE  WK-HED-F304
***************ホーマック資材
               WHEN  000880
               WHEN  000882
               WHEN  000883
               MOVE      HED-F063    TO   WK-DEPB05
               EVALUATE  WK-HED-F17
                   WHEN  2
                         MOVE   WK-DEPB-REC TO JCAA-REC
                         WRITE  JCAA-REC
                         ADD    1    TO   JCA-CNTA
                   WHEN  3
                         MOVE   WK-DEPB-REC TO JCAB-REC
                         WRITE  JCAB-REC
                         ADD    1    TO   JCA-CNTB
                   WHEN  4
                         MOVE   WK-DEPB-REC TO JCAC-REC
                         WRITE  JCAC-REC
                         ADD    1    TO   JCA-CNTC
                   WHEN  OTHER
                         MOVE   WK-DEPB-REC TO JCAH-REC
                         WRITE  JCAH-REC
                         ADD    1    TO   JCA-CNTH
               END-EVALUATE
***************ホーマック植物
               WHEN  001427
               WHEN  014272
               WHEN  014273
               MOVE      HED-F063    TO   WK-DEPB05
               EVALUATE  WK-HED-F17
                   WHEN  2
                         MOVE   WK-DEPB-REC TO JCAE-REC
                         WRITE  JCAE-REC
                         ADD    1    TO   JCA-CNTE
                   WHEN  3
                         MOVE   WK-DEPB-REC TO JCAF-REC
                         WRITE  JCAF-REC
                         ADD    1    TO   JCA-CNTF
                   WHEN  4
                         MOVE   WK-DEPB-REC TO JCAG-REC
                         WRITE  JCAG-REC
                         ADD    1    TO   JCA-CNTG
                   WHEN  OTHER
                         MOVE   WK-DEPB-REC TO JCAH-REC
                         WRITE  JCAH-REC
                         ADD    1    TO   JCA-CNTH
               END-EVALUATE
***************カーマ資材
               WHEN  013938
                     MOVE   HED-F03(3:7)   TO   WK-DEPB03
                     MOVE   WK-DEPB-REC TO KHMA-REC
                     WRITE  KHMA-REC
                     ADD    1    TO   JCA-CNTKAMAA
***************カーマ植物
               WHEN  017137
                     MOVE   HED-F03(3:7)   TO   WK-DEPB03
                     MOVE   WK-DEPB-REC TO KHMB-REC
                     WRITE  KHMB-REC
                     ADD    1    TO   JCA-CNTKAMAB
***************ダイキ
               WHEN  100403   WHEN  100441  WHEN   100434
                     MOVE   WK-DEPB-REC TO DAI1-REC
                     WRITE  DAI1-REC
                     ADD    1    TO   JCA-CNTDAIK1
***************ダイキ
               WHEN  100427
                     MOVE   WK-DEPB-REC TO DAI2-REC
                     WRITE  DAI2-REC
                     ADD    1    TO   JCA-CNTDAIK2
***************対象外
               WHEN  OTHER
                     MOVE   WK-DEPB-REC TO JCAH-REC
                     WRITE  JCAH-REC
                     ADD    1    TO   JCA-CNTH
           END-EVALUATE

     END-IF.
*明細行
     IF    EDI-01  =  "DT"
***********ＪＣＡフォーマット初期化(明細）
           MOVE      SPACE       TO   WK-DEPD-REC
           INITIALIZE                 WK-DEPD-REC
***********ワークエリア初期化
           MOVE      SPACE       TO   MEI-REC
           INITIALIZE                 MEI-REC
***********ヘッダ情報→ワークにセット
           MOVE      EDI-REC     TO   MEI-REC
***********ＪＣＡフォーマット項目セット
           MOVE      "L"         TO   WK-DEPD01
***********MOVE      1           TO   WK-DEPD02
           MOVE      MEI-F03     TO   WK-DEPD02
           MOVE      MEI-F04     TO   WK-DEPD031
           MOVE      SPACE       TO   WK-DEPD032
           MOVE      MEI-F07     TO   WK-DEPD04
           MOVE      MEI-F10     TO   WK-DEPD05
           MOVE      MEI-F122    TO   WK-DEPD051
           COMPUTE WK-DEPD06 = MEI-F15 / 100
           MOVE      MEI-F13     TO   WK-DEPD07
           MOVE      MEI-F16     TO   WK-DEPD08
           MOVE      MEI-F14     TO   WK-DEPD09
           MOVE      MEI-F02     TO   WK-DEPD10
           MOVE      SPACE       TO   WK-DEPD11
***********IF        WK-DEPB081 = 899 OR 929
                     MOVE      MEI-F051    TO   WK-DEPD11
***********END-IF
***********取引先別に処理を振り分けます。
           EVALUATE  WK-HED-F304
***************ホーマック資材
               WHEN  000880
               WHEN  000882
               WHEN  000883
               EVALUATE  WK-HED-F17
                   WHEN  2
                         MOVE   WK-DEPD-REC TO JCAA-REC
                         WRITE  JCAA-REC
                         ADD    1    TO   JCA-CNTA
                   WHEN  3
                         MOVE   WK-DEPD-REC TO JCAB-REC
                         WRITE  JCAB-REC
                         ADD    1    TO   JCA-CNTB
                   WHEN  4
                         MOVE   WK-DEPD-REC TO JCAC-REC
                         WRITE  JCAC-REC
                         ADD    1    TO   JCA-CNTC
                   WHEN  OTHER
                         MOVE   WK-DEPD-REC TO JCAH-REC
                         WRITE  JCAH-REC
                         ADD    1    TO   JCA-CNTH
               END-EVALUATE
***************ホーマック植物
               WHEN  001427
               WHEN  014272
               WHEN  014273
               EVALUATE  WK-HED-F17
                   WHEN  2
                         MOVE   WK-DEPD-REC TO JCAE-REC
                         WRITE  JCAE-REC
                         ADD    1    TO   JCA-CNTE
                   WHEN  3
                         MOVE   WK-DEPD-REC TO JCAF-REC
                         WRITE  JCAF-REC
                         ADD    1    TO   JCA-CNTF
                   WHEN  4
                         MOVE   WK-DEPD-REC TO JCAG-REC
                         WRITE  JCAG-REC
                         ADD    1    TO   JCA-CNTG
                   WHEN  OTHER
                         MOVE   WK-DEPD-REC TO JCAH-REC
                         WRITE  JCAH-REC
                         ADD    1    TO   JCA-CNTH
               END-EVALUATE
***************カーマ資材
               WHEN  013938
                     MOVE   WK-DEPD-REC TO KHMA-REC
                     WRITE  KHMA-REC
                     ADD    1    TO   JCA-CNTKAMAA
***************カーマ植物
               WHEN  017137
                     MOVE   WK-DEPD-REC TO KHMB-REC
                     WRITE  KHMB-REC
                     ADD    1    TO   JCA-CNTKAMAB
***************ダイキ
               WHEN  100403  WHEN  100441  WHEN  100434
                     MOVE   WK-DEPD-REC TO DAI1-REC
                     WRITE  DAI1-REC
                     ADD    1    TO   JCA-CNTDAIK1
***************ダイキ
               WHEN  100427
                     MOVE   WK-DEPD-REC TO DAI2-REC
                     WRITE  DAI2-REC
                     ADD    1    TO   JCA-CNTDAIK2
***************対象外
               WHEN  OTHER
                     MOVE   WK-DEPD-REC TO JCAH-REC
                     WRITE  JCAH-REC
                     ADD    1    TO   JCA-CNTH
           END-EVALUATE
***********ＪＣＡフォーマット初期化(トレイラー）
           MOVE      SPACE       TO   WK-DEPT-REC
           INITIALIZE                 WK-DEPT-REC
           MOVE      "T"         TO   WK-DEPT01
           MOVE      ZERO        TO   WK-DEPT02
           MOVE      MEI-F13     TO   WK-DEPT03
           MOVE      MEI-F14     TO   WK-DEPT04
           MOVE      SPACE       TO   WK-DEPT05
***********取引先別に処理を振り分けます。
           EVALUATE  WK-HED-F304
***************ホーマック資材
               WHEN  000880
               WHEN  000882
               WHEN  000883
               EVALUATE  WK-HED-F17
                   WHEN  2
                         MOVE   WK-DEPT-REC TO JCAA-REC
                         WRITE  JCAA-REC
                         ADD    1    TO   JCA-CNTA
                   WHEN  3
                         MOVE   WK-DEPT-REC TO JCAB-REC
                         WRITE  JCAB-REC
                         ADD    1    TO   JCA-CNTB
                   WHEN  4
                         MOVE   WK-DEPT-REC TO JCAC-REC
                         WRITE  JCAC-REC
                         ADD    1    TO   JCA-CNTC
                   WHEN  OTHER
                         MOVE   WK-DEPT-REC TO JCAH-REC
                         WRITE  JCAH-REC
                         ADD    1    TO   JCA-CNTH
               END-EVALUATE
***************ホーマック植物
               WHEN  001427
               WHEN  014272
               WHEN  014273
               EVALUATE  WK-HED-F17
                   WHEN  2
                         MOVE   WK-DEPT-REC TO JCAE-REC
                         WRITE  JCAE-REC
                         ADD    1    TO   JCA-CNTE
                   WHEN  3
                         MOVE   WK-DEPT-REC TO JCAF-REC
                         WRITE  JCAF-REC
                         ADD    1    TO   JCA-CNTF
                   WHEN  4
                         MOVE   WK-DEPT-REC TO JCAG-REC
                         WRITE  JCAG-REC
                         ADD    1    TO   JCA-CNTG
                   WHEN  OTHER
                         MOVE   WK-DEPT-REC TO JCAH-REC
                         WRITE  JCAH-REC
                         ADD    1    TO   JCA-CNTH
               END-EVALUATE
***************カーマ資材
               WHEN  013938
                     MOVE   WK-DEPT-REC TO KHMA-REC
                     WRITE  KHMA-REC
                     ADD    1    TO   JCA-CNTKAMAA
***************カーマ植物
               WHEN  017137
                     MOVE   WK-DEPT-REC TO KHMB-REC
                     WRITE  KHMB-REC
                     ADD    1    TO   JCA-CNTKAMAB
***************ダイキ資材
               WHEN  100403  WHEN  100441   WHEN   100434
                     MOVE   WK-DEPT-REC TO DAI1-REC
                     WRITE  DAI1-REC
                     ADD    1    TO   JCA-CNTDAIK1
***************ダイキ植物
               WHEN  100427
                     MOVE   WK-DEPT-REC TO DAI2-REC
                     WRITE  DAI2-REC
                     ADD    1    TO   JCA-CNTDAIK2
***************対象外
               WHEN  OTHER
                     MOVE   WK-DEPT-REC TO JCAH-REC
                     WRITE  JCAH-REC
                     ADD    1    TO   JCA-CNTH
           END-EVALUATE
     END-IF.
*
     PERFORM JEDICOS-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ファイル読み込み　　　　　　　　　　　　　　　　*
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
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     CLOSE     JEDICOS JCAFILEA JCAFILEB JCAFILEC JCAKAMAA.
     CLOSE     JCAFILEE JCAFILEF JCAFILEG JCAKAMAB.
     CLOSE     JCADAIK1 JCADAIK2 JCAFILEH.
*
     DISPLAY NC"＃ＨＣ資材　北海道ＣＮＴ＝"  JCA-CNTA
                                             UPON CONS.
     DISPLAY NC"＃ＨＣ資材　東北　ＣＮＴ＝"  JCA-CNTB
                                             UPON CONS.
     DISPLAY NC"＃ＨＣ資材　関東　ＣＮＴ＝"  JCA-CNTC
                                             UPON CONS.
     DISPLAY NC"＃カーマ資材　　　ＣＮＴ＝"  JCA-CNTKAMAA
                                             UPON CONS.
     DISPLAY NC"＃ＨＣ植物　北海道ＣＮＴ＝"  JCA-CNTE
                                             UPON CONS.
     DISPLAY NC"＃ＨＣ植物　東北　ＣＮＴ＝"  JCA-CNTF
                                             UPON CONS.
     DISPLAY NC"＃ＨＣ植物　関東　ＣＮＴ＝"  JCA-CNTG
                                             UPON CONS.
     DISPLAY NC"＃カーマ植物　　　ＣＮＴ＝"  JCA-CNTKAMAB
                                             UPON CONS.
     DISPLAY NC"＃ダイキ資材　　　ＣＮＴ＝"  JCA-CNTDAIK1
                                             UPON CONS.
     DISPLAY NC"＃ダイキ植物　　　ＣＮＴ＝"  JCA-CNTDAIK2
                                             UPON CONS.
     DISPLAY NC"＃対象外　　　　　ＣＮＴ＝"  JCA-CNTH
                                             UPON CONS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
