# SJU2030B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SJU2030B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受配信管理システム　　　　　　　　*
*    業務名　　　　　　　：　ＥＯＳ管理                        *
*    モジュール名　　　　：　自動バックアップ機能(2048BYTE)    *
*                        ：　ＳＪＵ２０３０Ｂ                  *
*    作成日／更新日　　　：　2005/09/14                        *
*    作成者／更新者　　　：　ＮＡＶ武井                        *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SJU2030B.
 AUTHOR.               TAKEI.
 DATE-WRITTEN.         05/09/14.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*受信ファイル
     SELECT  CVCSGF    ASSIGN    TO        DA-01-S-CVCSG003
                       ACCESS    MODE      SEQUENTIAL
                       FILE      STATUS    CVC-ST.
*バックアップファイル
     SELECT  BKCVCF    ASSIGN    TO        DA-01-S-BKCVCS03
                       ACCESS    MODE      SEQUENTIAL
                       FILE      STATUS    BKC-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 受信ファイル(2048)                                 *
****************************************************************
 FD  CVCSGF
                       LABEL     RECORD    IS   STANDARD
                       BLOCK     CONTAINS  1    RECORDS.
 01  CVC-REC.
     03  CVC-DATA                PIC  X(2048).
*
****************************************************************
*    FILE = バックアップファイル                               *
****************************************************************
 FD  BKCVCF
                       LABEL     RECORD    IS   STANDARD
                       BLOCK     CONTAINS  1    RECORDS.
 01  BKC-REC.
     03  BKC-BNO.
       05  BKC-F01               PIC  X(08).
       05  BKC-F02               PIC  X(04).
       05  BKC-F03               PIC  X(08).
     03  BKC-DATA                PIC  X(2048).
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  CVC-ST                   PIC  X(02).
     03  BKC-ST                   PIC  X(02).
*フラグ、カウンター、その他
 01  WORK-AREA.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  WT-IN-CNT                PIC  9(05)  VALUE  ZERO.
     03  WT-CNT                   PIC  9(05)  VALUE  ZERO.
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  CVC-ERR           PIC N(15) VALUE
                        NC"受信ファイルエラー".
     03  BKC-ERR           PIC N(15) VALUE
                        NC"バックアップファイルエラー".
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
*01  LINK-BNO.
 01  LINK-B01                     PIC  X(08).
 01  LINK-B02                     PIC  X(04).
 01  LINK-B03                     PIC  X(08).
*
**************************************************************
 PROCEDURE        DIVISION  USING LINK-B01 LINK-B02 LINK-B03.
**************************************************************
 DECLARATIVES.
 CVC-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE CVCSGF.
     MOVE        CVC-ST    TO        E-ST.
     MOVE        "CVCSGF"  TO      E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     CVC-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 BKC-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE BKCVCF.
     MOVE        BKC-ST    TO        E-ST.
     MOVE        "BKCVCF"  TO      E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     BKC-ERR   UPON      CONS.
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
     OPEN  INPUT  CVCSGF.
     DISPLAY "LINK-DATE = " LINK-B01    UPON  CONS.
     DISPLAY "LINK-TIME = " LINK-B02    UPON  CONS.
     DISPLAY "LINK-CODE = " LINK-B03    UPON  CONS.
*受信ファイル初期読込み
     PERFORM   CVCSGF-READ-SEC.
*データ存在チェック
     IF  END-FLG  =  "END"
         DISPLAY NC"バックアップ対象なし" UPON CONS
     ELSE
         OPEN  EXTEND BKCVCF
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*    2.0       メイン処理                                      *
****************************************************************
 MAIN-SEC              SECTION.
*バックアップレコード初期化
     MOVE      SPACE      TO      BKC-REC.
     INITIALIZE                   BKC-REC.
*項目セット
*受信日付
     MOVE      LINK-B01     TO    BKC-F01.
*受信時間
     MOVE      LINK-B02     TO    BKC-F02.
*取引先コード
     MOVE      LINK-B03     TO    BKC-F03.
*データ部
     MOVE      CVC-DATA     TO    BKC-DATA.
*バックアップレコード作成
     WRITE      BKC-REC.
     ADD        1           TO   WT-CNT.
*受信ファイル読込み
     PERFORM    CVCSGF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*    3.0       終了処理                                        *
****************************************************************
 END-SEC               SECTION.
*ファイルのＣＬＯＳＥ
     CLOSE        CVCSGF.
     IF     WT-CNT  >  ZERO
            CLOSE BKCVCF
     END-IF.
*
 END-EXIT.
     EXIT.
****************************************************************
*  受信ファイル読込み                                          *
****************************************************************
 CVCSGF-READ-SEC      SECTION.
*
     READ  CVCSGF  AT  END
           MOVE     "END"     TO     END-FLG
           GO                 TO     CVCSGF-READ-EXIT
     END-READ.
     ADD   1    TO   WT-IN-CNT.
*
 CVCSGF-READ-EXIT.
     EXIT.

```
