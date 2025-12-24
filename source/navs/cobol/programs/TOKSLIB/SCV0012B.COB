****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受配信管理システム　　　　　　　　*
*    業務名　　　　　　　：　ＣＶＣＳ管理                      *
*    モジュール名　　　　：　ＥＯＳ管理Ｆ→取引先管理Ｆ作成    *
*    作成日／更新日　　　：　99/11/18                          *
*    作成者／更新者　　　：　ＮＡＶ高橋                        *
*    処理概要　　　　　　：　Ｋ側転送用　　　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SCV0012B.
 AUTHOR.               TAKAHASHI.
 DATE-WRITTEN.         99/11/18.
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
     SELECT  JHMEOSF   ASSIGN    TO        DA-01-VI-JHMEOSF
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
     SELECT  KANRIWK   ASSIGN    TO        DA-01-S-KANRIWK
                       ACCESS    MODE      SEQUENTIAL
                       FILE      STATUS    KAN-ST.
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
*    FILE = 管理ファイルワーク                                 *
****************************************************************
 FD  KANRIWK
                       BLOCK     CONTAINS   5   RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      KANRIWK1  OF   XFDLIB
                       JOINING   KAN       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  EOS-ST                   PIC  X(02).
     03  KAN-ST                   PIC  X(02).
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
     03  KAN-ERR           PIC N(15) VALUE
                        NC"管理ファイルワークエラー".
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
 KAN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE KANRIWK.
     MOVE        KAN-ST    TO        E-ST.
     MOVE       "KANRIWK"  TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     KAN-ERR   UPON      CONS.
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
               OPEN  OUTPUT KANRIWK
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*    2.0       メイン処理                                      *
****************************************************************
 MAIN-SEC              SECTION.
*取引先管理Ｆ初期化
     MOVE      SPACE      TO      KAN-REC.
     INITIALIZE                   KAN-REC.
*レコードセット
     MOVE      EOS-REC      TO   KAN-REC.
*取引先名
     MOVE      EOS-F03      TO   TOK-F01.
     READ      HTOKMS
               INVALID
               MOVE   ALL "*"   TO  KAN-F16
               NOT INVALID
               MOVE   TOK-F04   TO  KAN-F16
     END-READ.
*取引先管理ファイル作成
     WRITE      KAN-REC.
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
            CLOSE KANRIWK
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
     IF    EOS-F04  NOT =  LINK-KAISEN
           GO                 TO     JHMEOSF-READ-SEC
     END-IF.
*
 JHMEOSF-READ-EXIT.
     EXIT.
*****************<<  SCV0020B   END PROGRAM  >>******************
