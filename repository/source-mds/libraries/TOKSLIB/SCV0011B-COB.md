# SCV0011B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SCV0011B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受配信管理システム　　　　　　　　*
*    業務名　　　　　　　：　ＣＶＣＳ管理                      *
*    モジュール名　　　　：　ＥＯＳ管理Ｆ→取引先管理Ｆ作成    *
*    作成日／更新日　　　：　99/11/19                          *
*    作成者／更新者　　　：　ＮＡＶ高橋                        *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SCV0011B.
 AUTHOR.               TAKAHASHI.
 DATE-WRITTEN.         99/11/19.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*ＥＯＳ管理マスタ
     SELECT  KANRIWK   ASSIGN    TO        DA-01-S-KANRIWK
                       ACCESS    MODE      SEQUENTIAL
                       FILE      STATUS    KAN-ST.
*集信／配信取引先管理ファイル
     SELECT  CVCSS013  ASSIGN    TO        DA-01-S-CVCSS013
                       ACCESS    MODE      SEQUENTIAL
                       FILE      STATUS    CVC-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 管理ファイルワーク                                 *
****************************************************************
 FD  KANRIWK           BLOCK     CONTAINS  5    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      KANRIWK1  OF   XFDLIB
                       JOINING   KAN       AS   PREFIX.
*
****************************************************************
*    FILE = 集信／配信取引先管理ファイル                       *
****************************************************************
 FD  CVCSS013          BLOCK     CONTAINS  4    RECORDS.
 01  CVC-REC.
     03  CVC-ID                  PIC  X(02).
     03  CVC-CTR                 PIC  X(06).
     03  CVC-CTR-SKBET           PIC  X(01).
     03  CVC-TORICD              PIC  X(08).
     03  CVC-SKBET               PIC  X(06).
     03  CVC-DATA                PIC  X(02).
     03  CVC-TORINM              PIC  X(19).
     03  CVC-NCU                 PIC  X(01).
     03  CVC-TEL                 PIC  X(17).
     03  CVC-BLK                 PIC  X(04).
     03  CVC-DATAFLG             PIC  X(01).
     03  CVC-FILLER              PIC  X(13).
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  KAN-ST                   PIC  X(02).
     03  CVC-ST                   PIC  X(02).
*フラグ、カウンター、その他
 01  WORK-AREA.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  TOK-INV-FLG              PIC  X(03)  VALUE  SPACE.
     03  WT-CNT                   PIC  9(03)  VALUE  ZERO.
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  KAN-ERR           PIC N(15) VALUE
                        NC"管理ファイルエラー".
     03  CVS-ERR           PIC N(15) VALUE
                        NC"取引先管理Ｆエラー".
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
***  エラーファイル名
 01  ERR-FILE.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-FILE   => ".
     03  E-FILE                   PIC  X(08).
***  エラーステータス名
 01  ERR-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-STATUS => ".
     03  E-ST                     PIC  9(02).
*
**************************************************************
 PROCEDURE             DIVISION.
**************************************************************
 DECLARATIVES.
 KAN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE KANRIWK.
     MOVE        KAN-ST    TO        E-ST.
     MOVE        "KANRIWK" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     KAN-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 CVS-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE CVCSS013.
     MOVE        CVC-ST    TO        E-ST.
     MOVE       "CVCSS013" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     CVS-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*    0.0      PROCESS     MODULE                               *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS-START"     TO   S-NAME.
***  プログラム制御
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC      UNTIL   END-FLG  =  "END".
     PERFORM   END-SEC.
***
     STOP  RUN.
***
 PROCESS-START-EXIT.
     EXIT.
****************************************************************
*    1.0       初期処理                                        *
****************************************************************
 INIT-SEC              SECTION.
*ファイルのＯＰＥＮ
     OPEN  INPUT  KANRIWK.
*管理ファイルワーク初期読込み
     PERFORM   KANRIWK-READ-SEC.
*データ存在チェック
     IF        END-FLG  =  "END"
               DISPLAY NC"作成対象未登録" UPON CONS
     ELSE
               OPEN  OUTPUT CVCSS013
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*    2.0       メイン処理                                      *
****************************************************************
 MAIN-SEC              SECTION.
*取引先管理Ｆ初期化
     MOVE      SPACE      TO      CVC-REC.
     INITIALIZE                   CVC-REC.
*項目セット
*ＩＤ
     MOVE      KAN-F05      TO   CVC-ID.
*センターコード
     MOVE      KAN-F06      TO   CVC-CTR
*センター識別子
     MOVE      KAN-F07      TO   CVC-CTR-SKBET.
*取引先コード
     MOVE      KAN-F08      TO   CVC-TORICD.
*識別子
     MOVE      KAN-F09      TO   CVC-SKBET.
*データ種類
     MOVE      KAN-F10      TO   CVC-DATA.
*取引先名
     MOVE      KAN-F16      TO   CVC-TORINM.
*ＮＣＵフラグ
     MOVE      "1"          TO   CVC-NCU.
*電話番号
     MOVE      KAN-F11      TO   CVC-TEL.
*電送ブロックサイズ
     MOVE      KAN-F12      TO   CVC-BLK.
*データ長フラグ
     MOVE      "1"          TO   CVC-DATAFLG.
*取引先管理ファイル作成
     WRITE      CVC-REC.
     ADD        1           TO   WT-CNT.
*管理ファイルワーク読込み
     PERFORM    KANRIWK-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*    3.0       終了処理                                        *
****************************************************************
 END-SEC               SECTION.
*ファイルのＣＬＯＳＥ
     CLOSE        KANRIWK.
     IF     WT-CNT  >  ZERO
            CLOSE CVCSS013
     END-IF.
*
 END-EXIT.
     EXIT.
****************************************************************
*  共通      管理ファイルワーク読込み                        *
****************************************************************
 KANRIWK-READ-SEC      SECTION.
*管理ファイルワーク読込み
     READ  KANRIWK  AT  END
           MOVE     "END"     TO     END-FLG
     END-READ.
*
 KANRIWK-READ-EXIT.
     EXIT.
*****************<<  SCV0011B   END PROGRAM  >>******************

```
