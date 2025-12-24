# SCV0010B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SCV0010B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受配信管理システム　　　　　　　　*
*    業務名　　　　　　　：　ＣＶＣＳ管理                      *
*    モジュール名　　　　：　ＥＯＳ管理Ｆ→取引先管理Ｆ作成    *
*    作成日／更新日　　　：　99/09/16                          *
*    作成者／更新者　　　：　ＮＡＶ高橋                        *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SCV0010B.
 AUTHOR.               TAKAHASHI.
 DATE-WRITTEN.         99/09/16.
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
     SELECT  JHMEOSF   ASSIGN    TO        DA-01-VI-JHMEOSL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       EOS-F01
                                           EOS-F02
                                           EOS-F03
                       FILE      STATUS    EOS-ST.
*取引先マスタ
     SELECT  HTOKMS    ASSIGN    TO        DA-01-VI-HTOKMS
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       TOK-F01
                       FILE      STATUS    TOK-ST.
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
*    FILE = ＥＯＳ管理マスタ                                   *
****************************************************************
 FD  JHMEOSF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      JHMEOSF   OF   XFDLIB
                       JOINING   EOS       AS   PREFIX.
*
****************************************************************
*    FILE = 取引先マスタ                                       *
****************************************************************
 FD  HTOKMS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HTOKMS    OF   XFDLIB
                       JOINING   TOK       AS   PREFIX.
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
     03  EOS-ST                   PIC  X(02).
     03  CVC-ST                   PIC  X(02).
     03  TOK-ST                   PIC  X(02).
*フラグ、カウンター、その他
 01  WORK-AREA.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  TOK-INV-FLG              PIC  X(03)  VALUE  SPACE.
     03  WT-CNT                   PIC  9(03)  VALUE  ZERO.
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  EOS-ERR           PIC N(15) VALUE
                        NC"ＥＯＳ管理マスタエラー".
     03  CVS-ERR           PIC N(15) VALUE
                        NC"取引先管理Ｆエラー".
     03  TOK-ERR           PIC N(15) VALUE
                        NC"取引先マスタエラー".
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
**************************************************************
 LINKAGE               SECTION.
 01  LINK-KAISEN                  PIC  X(01).
*
**************************************************************
 PROCEDURE             DIVISION   USING   LINK-KAISEN.
**************************************************************
 DECLARATIVES.
 EOS-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JHMEOSF.
     MOVE        EOS-ST    TO        E-ST.
     MOVE        "JHMEOSF" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     EOS-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTOKMS.
     MOVE        TOK-ST    TO        E-ST.
     MOVE        "HTOKMS"  TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     TOK-ERR   UPON      CONS.
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
     OPEN  INPUT  JHMEOSF  HTOKMS.
*ＥＯＳ管理Ｆ初期読込み
     PERFORM   JHMEOSF-READ-SEC.
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
     MOVE      EOS-F05      TO   CVC-ID.
*センターコード
     MOVE      EOS-F06      TO   CVC-CTR
*センター識別子
     MOVE      EOS-F07      TO   CVC-CTR-SKBET.
*取引先コード
     MOVE      EOS-F08      TO   CVC-TORICD.
*識別子
     MOVE      EOS-F09      TO   CVC-SKBET.
*データ種類
     MOVE      EOS-F10      TO   CVC-DATA.
*取引先名
     MOVE      EOS-F03      TO   TOK-F01.
     READ      HTOKMS
               INVALID
               MOVE   ALL "*"   TO  CVC-TORINM
               NOT INVALID
               MOVE   TOK-F04   TO  CVC-TORINM
     END-READ.
**** MOVE      EOS-F11      TO   CVC-TORINM.
*ＮＣＵフラグ
     MOVE      "1"          TO   CVC-NCU.
*電話番号
     MOVE      EOS-F11      TO   CVC-TEL.
*電送ブロックサイズ
     MOVE      EOS-F12      TO   CVC-BLK.
*データ長フラグ
     MOVE      "1"          TO   CVC-DATAFLG.
*取引先管理ファイル作成
     WRITE      CVC-REC.
     ADD        1           TO   WT-CNT.
*ＥＯＳ管理マスタ読込み
     PERFORM    JHMEOSF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*    3.0       終了処理                                        *
****************************************************************
 END-SEC               SECTION.
*ファイルのＣＬＯＳＥ
     CLOSE        JHMEOSF  HTOKMS.
     IF     WT-CNT  >  ZERO
            CLOSE CVCSS013
     END-IF.
*
 END-EXIT.
     EXIT.
****************************************************************
*  共通      ＥＯＳ管理マスタ読込み                          *
****************************************************************
 JHMEOSF-READ-SEC      SECTION.
*ＥＯＳ管理マスタ読込み
     READ  JHMEOSF  AT  END
           MOVE     "END"     TO     END-FLG
           GO                 TO     JHMEOSF-READ-EXIT
     END-READ.
*回線種別チェック
*****DISPLAY "EOS-F04      = " EOS-F04      UPON CONS.
*****DISPLAY "LINK-KAISEN  = " LINK-KAISEN  UPON CONS.
     IF    EOS-F04  NOT =  LINK-KAISEN
           GO                 TO     JHMEOSF-READ-SEC
     END-IF.
*
 JHMEOSF-READ-EXIT.
     EXIT.
*****************<<  SCV0020B   END PROGRAM  >>******************

```
