# SJH0005L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SJH0005L.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受配信                            *
*    業務名　　　　　　　：　基本スケジュール                  *
*    モジュール名　　　　：　基本スケジュールマスタリスト      *
*    作成日／更新日　　　：　99/09/07                          *
*    作成者／更新者　　　：　ＮＡＶ萩原                        *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SJH0005L.
 AUTHOR.                HAGIWARA.
 DATE-WRITTEN.          99/09/07.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITU.
 OBJECT-COMPUTER.       FUJITU.
 SPECIAL-NAMES.
         CONSOLE        IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*基本スケジュールマスタ
     SELECT     JHMKIHF    ASSIGN    TO        JHMKIHL1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      SEQUENTIAL
                           RECORD    KEY       KIH-F01
                                               KIH-F02
                           FILE      STATUS    KIH-ST.
*取引先マスタ
     SELECT     HTOKMS     ASSIGN    TO        TOKMS2
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       TOK-F01
                           FILE      STATUS    TOK-ST.
*ＥＯＳ管理マスタ
     SELECT     JHMEOSF    ASSIGN    TO        JHMEOSL1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      DYNAMIC
                           RECORD    KEY       EOS-F01
                                               EOS-F02
                                               EOS-F03
                           FILE      STATUS    EOS-ST.
*プリント定義ファイル
     SELECT     PRTFILE    ASSIGN    TO        GS-PRT
*               SYMBOLIC DESTINATION IS        "PRT"
                PROCESSING MODE      IS        PRT-PROC
                GROUP                IS        PRT-GROUP
                FORMAT               IS        PRT-FORMAT
                CONTROL              IS        PRT-CNTL
                FILE STATUS          IS        PRT-ST.
****************************************************************
 DATA                      DIVISION.
****************************************************************
 FILE                      SECTION.
****************************************************************
*  FILE=基本スケジュールマスタ                                 *
****************************************************************
 FD  JHMKIHF
     LABEL       RECORD    IS        STANDARD.
     COPY        JHMKIHF   OF        XFDLIB
     JOINING     KIH       AS        PREFIX.
****************************************************************
*  FILE= 取引先マスタ                                          *
****************************************************************
 FD  HTOKMS
     BLOCK       CONTAINS   8        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        HTOKMS    OF        XFDLIB
     JOINING     TOK       AS        PREFIX.
****************************************************************
*  FILE=ＥＯＳ管理マスタ                                       *
****************************************************************
 FD  JHMEOSF
     LABEL       RECORD    IS        STANDARD.
     COPY        JHMEOSF   OF        XFDLIB
     JOINING     EOS       AS        PREFIX.
****************************************************************
*  FILE=プリントファイル                                       *
****************************************************************
 FD  PRTFILE
     LABEL       RECORD    IS        OMITTED.
     COPY        FJH00051  OF        XMDLIB
     JOINING     PRT       AS        PREFIX.
******************************************************************
 WORKING-STORAGE           SECTION.
******************************************************************
*
***  ｽﾃｰﾀｽｴﾘｱ
 01  FILE-STATUS.
     03  KIH-ST              PIC X(02).
     03  TOK-ST              PIC X(02).
     03  EOS-ST              PIC X(02).
     03  PRT-ST              PIC X(02).
***  帳票制御用領域
 01  PRT-CONTROL.
     03  PRT-PROC            PIC  X(02).
     03  PRT-GROUP           PIC  X(08).
     03  PRT-FORMAT          PIC  X(08).
     03  PRT-CNTL            PIC  X(06).
***  ﾌﾗｸﾞｴﾘｱ
 01  FLG-AREA.
***  基本スケジュールマスタ用
     03  END-FLG             PIC X(03).
***  プログラム終了用
     03  PGMEND-FLG          PIC X(03).
***  ﾜｰｸｴﾘｱ
 01  WK-AREA.
     03  P-CNT               PIC 9(05) VALUE     ZERO.
     03  WK-TORICD           PIC 9(08) VALUE     ZERO.
     03  SYS-DATE            PIC 9(06) VALUE     ZERO.
     03  IX                  PIC 9(02) VALUE     ZERO.
     03  WK-YOBI             PIC 9(01) VALUE     ZERO.
*
 01  SEC-NAME.
     03  FILLER              PIC  X(05)     VALUE " *** ".
     03  S-NAME              PIC  X(30).
*
 01  FILE-ERR.
     03  KIH-ERR             PIC N(15) VALUE
                        NC"基本スケジュールマスタエラー".
     03  TOK-ERR             PIC N(15) VALUE
                        NC"取引先マスタエラー".
     03  EOS-ERR             PIC N(15) VALUE
                        NC"ＥＯＳ管理マスタエラー".
     03  PRT-ERR             PIC N(15) VALUE
                        NC"プリンターエラー".
***  日付取得
 01  WK-DATE8.
     03  WK-DATE8-YY1        PIC  9(02).
     03  WK-DATE8-YY2        PIC  9(06).
 01  WK-DATE8-R         REDEFINES  WK-DATE8.
     03  WK-YYYY             PIC  9(04).
     03  WK-MM               PIC  9(02).
     03  WK-DD               PIC  9(02).
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
****************************************************************
*             PROCEDURE           DIVISION                     *
****************************************************************
 PROCEDURE                           DIVISION.
 DECLARATIVES.
 KIH-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JHMKIHF.
     DISPLAY     KIH-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     KIH-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTOKMS.
     DISPLAY     TOK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     TOK-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 EOS-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JHMEOSF.
     DISPLAY     EOS-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     EOS-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 PRT-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE PRTFILE.
     DISPLAY     PRT-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     PRT-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END DECLARATIVES.
****************************************************************
*           　M A I N             M O D U L E         0.0      *
****************************************************************
 PROCESS-START             SECTION.
     MOVE        "PROCESS-START"     TO          S-NAME.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC  UNTIL     PGMEND-FLG = "END"
     PERFORM     END-SEC.
     STOP        RUN.
 PROC-EXIT.
     EXIT.
****************************************************************
*             初期処理                                1.0      *
****************************************************************
 INIT-SEC                  SECTION.
     MOVE        "INIT-SEC"          TO          S-NAME.
*システム日付・時刻の取得
     ACCEPT   SYS-DATE          FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     SYS-DATE            TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   WK-DATE8.
*ファイルＯＰＥＮ
     OPEN        INPUT       JHMKIHF
                             HTOKMS
                             JHMEOSF
                 OUTPUT      PRTFILE.
*
     MOVE        SPACE       TO   PRT-FJH00051.
***  日付
     MOVE     WK-YYYY             TO        PRT-DATEYY.
     MOVE     WK-MM               TO        PRT-DATEMM.
     MOVE     WK-DD               TO        PRT-DATEDD.
***  ページ_
     MOVE     1                   TO        PRT-PAGE.
*
*フラグの初期化
     MOVE        SPACE       TO   FLG-AREA.
     MOVE        SPACE       TO   END-FLG.
*ファイルＲＥＡＤ
     PERFORM     FL-READ-SEC.
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                              2.0      *
****************************************************************
 MAIN-SEC                  SECTION.
     MOVE          "MAIN-SEC"     TO        S-NAME.
*
     IF   END-FLG  NOT  =  "END"
***      曜日ブレイクチェック
          IF   KIH-F01  NOT = WK-YOBI
               MOVE     1         TO   IX
               MOVE     KIH-F01   TO   WK-YOBI
          ELSE
               ADD      1         TO   IX
          END-IF
***       明細出力部へ
          PERFORM       BODY-WT-SEC
***  基本ｽｹｼﾞｭｰﾙﾏｽﾀ=END の時，
     ELSE
***       ＥＯＳﾏｽﾀの区分が01（発注）でなければＥＮＤ
          IF   EOS-F01  = "01"
               ADD      1         TO   IX
          ELSE
               MOVE     "END"     TO   PGMEND-FLG
               GO       TO   MAIN-EXIT
          END-IF
***       問合せ項目出力部へ
          PERFORM       EOS-WT-SEC
     END-IF.

*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             ファイルＲＥＡＤ処理                    2.1      *
****************************************************************
 FL-READ-SEC            SECTION.
     MOVE     "FL-READ-SEC"       TO             S-NAME.
*
     READ     JHMKIHF   AT   END
         MOVE      "END"     TO        END-FLG
         MOVE      ZERO      TO        IX
         PERFORM   EOS-INIT-SEC
         GO                TO        FL-READ-EXIT
     END-READ.
*
 FL-READ-EXIT.
     EXIT.
****************************************************************
*             帳票出力処理                            2.2      *
****************************************************************
 BODY-WT-SEC                  SECTION.
     MOVE        "BODY-WT-SEC"      TO          S-NAME.
*
**** IF       WK-TORICD  =  KIH-F03
****          GO            TO       DT-010
**** ELSE
              MOVE   KIH-F03   TO        WK-TORICD.
**** END-IF.
*取引先ＲＥＡＤ
     MOVE        KIH-F03   TO        TOK-F01.
     READ     HTOKMS
     INVALID
          MOVE   SPACE     TO        PRT-TORINM(KIH-F01 IX)
     NOT INVALID
          MOVE   TOK-F03   TO        PRT-TORINM(KIH-F01 IX)
     END-READ.
*
 DT-010.
***  受信時間
     MOVE     KIH-F02(1:2)   TO      PRT-JIKAN1(KIH-F01 IX).
     MOVE     ":"            TO      PRT-KGRI(KIH-F01 IX).
     MOVE     KIH-F02(3:2)   TO      PRT-JIKAN2(KIH-F01 IX).
***  回線種別
     MOVE     "01"           TO      EOS-F01.
     MOVE     "1"            TO      EOS-F02.
     MOVE     KIH-F03        TO      EOS-F03.
     READ     JHMEOSF
        INVALID
              MOVE      ZERO      TO   PRT-KAICD(KIH-F01 IX)
        NOT INVALID
              MOVE      EOS-F04   TO   PRT-KAICD(KIH-F01 IX)
     END-READ.
*
     PERFORM  FL-READ-SEC.
*
 BODY-WT-EXIT.
     EXIT.
****************************************************************
*             問合せ項目初期処理（ＥＯＳ管理マスタ）　  2.3    *
****************************************************************
 EOS-INIT-SEC           SECTION.
     MOVE     "EOS-INIT-SEC"       TO             S-NAME.
     CLOSE    JHMEOSF.
     OPEN     INPUT     JHMEOSF.
*
     MOVE     "01"      TO   EOS-F01.
     MOVE     "1"       TO   EOS-F02.
     MOVE     ZERO      TO   EOS-F03.
     START    JHMEOSF   KEY  IS  >=    EOS-F01
                                       EOS-F02
                                       EOS-F03
     INVALID
              MOVE      "END"     TO   PGMEND-FLG
              GO   TO   EOS-INIT-EXIT
     NOT INVALID
              PERFORM   EOS-READ-SEC
     END-START.
*
 EOS-INIT-EXIT.
     EXIT.
****************************************************************
*             問合せ項目ＲＥＡＤ処理（ＥＯＳ管理マスタ）2.4    *
****************************************************************
 EOS-READ-SEC         SECTION.
     MOVE     "EOS-READ-SEC"      TO             S-NAME.
*
     READ     JHMEOSF   NEXT  AT  END
              MOVE      "END"     TO        PGMEND-FLG
     END-READ.

 EOS-READ-EXIT.
     EXIT.
****************************************************************
*             問合せ項目出力処理（ＥＯＳ管理マスタ）  2.5      *
****************************************************************
 EOS-WT-SEC             SECTION.
     MOVE     "EOS-WT-SEC"         TO             S-NAME.
*受配信区分＝２（配信）時，ＥＮＤ
     IF       EOS-F02   =    "2"
              MOVE      "END"      TO   PGMEND-FLG
              GO   TO   EOS-WT-EXIT
     END-IF.
*項目セット
***  取引先ＲＥＡＤ
     MOVE        EOS-F03   TO        PRT-EOSTCD(IX)  TOK-F01.
     READ     HTOKMS
     INVALID
          MOVE   SPACE     TO        PRT-EOSTNM(IX)
     NOT INVALID
          MOVE   TOK-F03   TO        PRT-EOSTNM(IX)
     END-READ.
***  連絡先名称
     MOVE     EOS-F151       TO   PRT-EOSREN(IX).
***  連絡先電話番号
     MOVE     EOS-F152       TO   PRT-EOSTEL(IX).
*
     PERFORM  EOS-READ-SEC.
*
 EOS-WT-EXIT.
     EXIT.
****************************************************************
*             終了処理                                3.0      *
****************************************************************
 END-SEC                   SECTION.
     MOVE        "END-SEC"           TO          S-NAME.
*
     MOVE        SPACE       TO   PRT-CONTROL.
     MOVE       "FJH00051"   TO   PRT-FORMAT.
     MOVE        "SCREEN"   TO         PRT-GROUP.
     WRITE       PRT-FJH00051.
*
     CLOSE       PRTFILE    JHMKIHF    HTOKMS    JHMEOSF.
 END-EXIT.
     EXIT.

```
