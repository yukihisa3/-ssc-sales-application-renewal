# SSY4007I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SSY4007I.COB`

## ソースコード

```cobol
**********************************************************
*                                                        *
*           セリ売上データ変換　確認画面　　　　         *
*           2000/02/28                                   *
**********************************************************
 IDENTIFICATION                      DIVISION.
 PROGRAM-ID.               SSY4007I.
 AUTHOR.                   T.TAKAHASHI.
 DATE-WRITTEN.             2000/02/28.
 ENVIRONMENT                         DIVISION.
 CONFIGURATION             SECTION.
 SOURCE-COMPUTER.
 OBJECT-COMPUTER.
 SPECIAL-NAMES.
     STATION     IS        STA.
***********************************************************
*             INPUT-OUTPUT                                *
***********************************************************
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*%* 表示ファイル *%*
     SELECT      DSPF      ASSIGN    TO        GS-DSPF
                           ORGANIZATION        SEQUENTIAL
                           DESTINATION        "DSP"
                           FORMAT              DSP-FORM
                           GROUP               DSP-GRP
                           PROCESSING          DSP-PROC
                           FUNCTION            PF-KEY
                           CONTROL             DSP-CONTROL
                           STATUS              DSP-ST.
******************************************************************
*             DATA                DIVISION                       *
******************************************************************
 DATA                      DIVISION.
 FILE                      SECTION.
 FD  DSPF
     LABEL       RECORD    IS        OMITTED.
     COPY        FSY40071  OF        XMDLIB.
******************************************************************
 WORKING-STORAGE       SECTION.
******************************************************************
*%*表示パラメータ*%*
 01  FORM-PARA.
     03  DSP-FORM          PIC X(08).
     03  DSP-PROC          PIC X(02).
     03  DSP-GRP           PIC X(08).
     03  PF-KEY            PIC X(04).
     03  DSP-CONTROL.
         05  DSP-CNTRL     PIC X(04).
         05  DSP-STR-PG    PIC X(02).
*
 01  IN-DATA               PIC X(01).
 01  FILE-STATUS.
     03  DSP-ST            PIC X(02).
     03  DSP-ST1           PIC X(04).
*
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-YS                    PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y                 PIC  9(02)  VALUE  ZERO.
         05  WK-M                 PIC  9(02)  VALUE  ZERO.
         05  WK-D                 PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE                 PIC  9(08).
*画面表示日付編集
 01  HEN-DATE.
     03  HEN-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD              PIC  9(02)  VALUE  ZERO.
*画面表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS              PIC  9(02)  VALUE  ZERO.
*
 01  FILE-ERR.
     03  DSP-ERR           PIC  N(10) VALUE
                        NC"画面ファイルエラー".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
******************************************************************
*                                                                *
*             PROCEDURE           DIVISION                       *
*                                                                *
******************************************************************
 PROCEDURE                           DIVISION.
 DECLARATIVES.
 DSP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DSPF.
     DISPLAY     DSP-ERR   UPON      STA.
     DISPLAY     DSP-ST    UPON      STA.
**** DISPLAY     DSP-ST1   UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
     STOP        RUN.
 END DECLARATIVES.
***********************************************************
*                     ＭＡＩＮ処理                        *
***********************************************************
 PROC-SEC                  SECTION.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC.
     PERFORM     END-SEC.
     STOP        RUN.
 PROC-EXIT.
     EXIT.
**********************************************************
*                      Ｉ Ｎ Ｉ Ｔ                       *
**********************************************************
 INIT-SEC                  SECTION.
     OPEN        I-O       DSPF.
*
*システム日付・時刻の取得
     ACCEPT   WK-DATE           FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-DATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   DATE-AREA.
*画面表示日付編集
     MOVE      SYS-DATE(1:4)      TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE(5:2)      TO   HEN-DATE-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DATE-DD.
*システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*
 INIT-EXIT.
     EXIT.
**********************************************************
*                       Ｅ Ｎ Ｄ                         *
**********************************************************
 END-SEC                   SECTION.
     CLOSE       DSPF.
 END-EXIT.
     EXIT.
***********************************************************
*                       画面処理                          *
***********************************************************
 MAIN-SEC                   SECTION.
     PERFORM     DSP-INT-SEC.
 MAIN-010.
     MOVE       "KAKU"     TO        DSP-GRP.
     PERFORM     DSP-RD-SEC.
     EVALUATE    PF-KEY
       WHEN
        "F005"
********** MOVE  250       TO        PROGRAM-STATUS
           MOVE  "155"     TO        PROGRAM-STATUS
       WHEN
        "E000"
           IF  (KAKU = "Y")
               CONTINUE
             ELSE
               MOVE     NC"Ｙで入力して下さい"  TO  ERRMSG
               MOVE       "ERRMSG"              TO  DSP-GRP
               PERFORM     DSP-WRT-SEC
               GO          TO        MAIN-010
           END-IF
       WHEN
         OTHER
           GO              TO        MAIN-010
     END-EVALUATE.
 MAIN-EXIT.
     EXIT.
***********************************************************
*                       画面出力                          *
***********************************************************
 DSP-WRT-SEC               SECTION.
     MOVE        SPACE     TO        DSP-PROC.
     WRITE       FSY40071.
 DSP-WRT-EXIT.
     EXIT.
***********************************************************
*                      画面入力                           *
***********************************************************
 DSP-RD-SEC                SECTION.
     MOVE       "NE"       TO        DSP-PROC.
     READ        DSPF      AT        END
                 STOP      RUN.
 DSP-RD-EXIT.
     EXIT.
***********************************************************
*                    画面初期設定                         *
***********************************************************
 DSP-INT-SEC               SECTION.
     MOVE        SPACE     TO        FSY40071.
     MOVE       "SCREEN"   TO        DSP-GRP.
     MOVE       "FSY40071" TO        DSP-FORM.
     MOVE       "CL"       TO        DSP-PROC.
     MOVE        HEN-DATE  TO        SDATE.
     MOVE        HEN-TIME  TO        STIME.
     PERFORM     DSP-WRT-SEC.
     MOVE       "Y"        TO        KAKU.
     MOVE       "KAKU"     TO        DSP-GRP.
     PERFORM     DSP-WRT-SEC.
 DSP-INT-EXIT.
     EXIT.

```
