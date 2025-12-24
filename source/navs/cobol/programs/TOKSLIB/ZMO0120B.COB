****************************************************************
*新基幹システムへ移行 1999/10/16 T.TAKAHASHI
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　
*    業務名　　　　　　　：　販売管理システム　　　　　　　　　
*    モジュール名　　　　：　売上データ未抽出チェックリスト　　
*    作成日／更新日　　　：　93/07/02
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　
*    処理概要　　　　　　：　売上データより，未抽出チェック　　
*                        ：　リストデータを抽出する　　　　　　
*    更新履歴            ：
*      2011/10/06 飯田/NAV 基幹サーバ統合
*
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            ZMO0120B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          93/07/02.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       K6500.
 OBJECT-COMPUTER.       K6500.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 表示ファイル >>--*
     SELECT   DSPF      ASSIGN         01-GS-DSPF
                        FORMAT         DSP-FMT
                        GROUP          DSP-GRP
                        PROCESSING     DSP-PRO
                        UNIT CONTROL   DSP-CON
                        FUNCTION       DSP-FNC
                        STATUS         DSP-ST.
*----<< 伝票データ >>--*
     SELECT   HDENJNL   ASSIGN         DA-01-VI-DENJNL6
                        ORGANIZATION   INDEXED
                        ACCESS         MODE SEQUENTIAL
                        RECORD         KEY  DEN-F277  DEN-F274
                                            DEN-F09   DEN-F02
                                       *> 2011/10/06,S  S.I/NAV
                                            DEN-F04   DEN-F051
                                            DEN-F07   DEN-F112
                                       *> 2011/10/06,E  S.I/NAV
                                            DEN-F03
                        STATUS         DEN-ST.
*----<< 伝票データ >>--*
     SELECT   OUTFILE   ASSIGN         DA-01-S-OUTFILE
                        ACCESS         MODE SEQUENTIAL
                        STATUS         OUT-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 表示ファイル >>--*
 FD  DSPF.
     COPY     ZMO0120   OF        XMDLIB
              JOINING   DSP       PREFIX.
*----<< 伝票データ >>--*
 FD  HDENJNL.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN       PREFIX.
*----<< 伝票データ >>--*
 FD  OUTFILE.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   OUT       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  DEN-ST             PIC  X(02).
 01  OUT-ST             PIC  X(02).
*----<< ﾃﾞｨｽﾌﾟﾚｲ ｺﾝﾄﾛｰﾙ ｴﾘｱ >>-*
 01  DSP-CNTL.
     03  DSP-ST         PIC  X(02).
     03  DSP-ST2        PIC  X(04).
     03  DSP-FMT        PIC  X(08).
     03  DSP-GRP        PIC  X(08).
     03  DSP-PRO        PIC  X(02).
     03  DSP-FNC        PIC  X(04).
     03  DSP-CON        PIC  X(06).
 01  FLAGS.
     03  MAIN-FLG       PIC  9(02).
     03  PFK-FLG        PIC  9(02).
     03  HIT-FLG        PIC  9(01).
     03  SKIP-FLG       PIC  9(01).
 01  COUNTERS.
     03  IN-CNT         PIC  9(05).
     03  SKIP-CNT       PIC  9(05).
     03  OUT-CNT        PIC  9(05).
*
*月末
 01  MATUBI-TBL.
     03  FILLER              PIC  X(24)  VALUE
                                 "312831303130313130313031".
 01  MATUBI-TBL-R    REDEFINES    MATUBI-TBL.
     03  WK-MATUBI           PIC  9(02)  OCCURS  12  TIMES.
 01  WORK-AREA.
     03  WK-SYSDT            PIC  9(06).
     03  WK-SYSDTR   REDEFINES    WK-SYSDT.
         05  WK-SYSYY        PIC  9(02).
         05  WK-SYSMM        PIC  9(02).
         05  WK-SYSDD        PIC  9(02).
     03  WK-NOUDT            PIC  9(08).
     03  WK-NOUDTR   REDEFINES    WK-NOUDT.
         05  WK-NOUYY        PIC  9(04).
         05  WK-NOUMM        PIC  9(02).
         05  WK-NOUDD        PIC  9(02).
     03  WK-SHO              PIC  9(04)  VALUE  ZERO.
     03  WK-AMARI            PIC  9(04)  VALUE  ZERO.
*----<< ﾌｱﾝｸｼﾖﾝ ｷｰ ｶﾞｲﾄﾞ >>-*
*ＰＦキー
 01  PFK-TBL.
     03  PFK-NO01            PIC  N(30)  VALUE
            NC"_終了".
     03  PFK-NO02            PIC  N(30)  VALUE
            NC"_取消　_終了　_項目戻り".
 01  TBL-PFK-R       REDEFINES    PFK-TBL.
     03  TBL-PFK             PIC  N(30)  OCCURS  2   TIMES.
****  メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "ZMO0120B".
       03  FILLER            PIC  X(10)  VALUE  " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
****  エラーメッセージコード  ***
 01  CODE-AREA.
     02  ERR-MSG-CD          PIC  9(02)  VALUE  ZERO.
****  エラーメッセージ  ***
 01  ERR-TAB.
     02  MSG-ERR1            PIC  N(28)  VALUE
            NC"無効ＰＦキーです。".
     02  MSG-ERR2            PIC  N(28)  VALUE
            NC"開始・終了コードに誤りがあります。".
     02  MSG-ERR3            PIC  N(28)  VALUE
            NC"納品日に誤りがあります".
     02  MSG-ERR4            PIC  N(28)  VALUE
            NC"Ｙで入力して下さい".
     02  MSG-ERR5            PIC  N(28)  VALUE
            NC"対象データがありません".
 01  ERR-MSG-ALL     REDEFINES    ERR-TAB.
     02  ERR-MSG             PIC  N(28)
                             OCCURS  5   TIMES.
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
 DECLARATIVES.
 FILEERR-SEC1                SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE      DSPF.
     MOVE     "DSPF    "     TO   ERR-FL-ID.
     MOVE     DSP-ST         TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     STOP     RUN.
**
 FILEERR-SEC2                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HDENJNL.
     MOVE     "HDENJNL"      TO   ERR-FL-ID.
     MOVE     DEN-ST         TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     STOP     RUN.
**
 FILEERR-SEC3                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    OUTFILE.
     MOVE     "OUTFILE"      TO   ERR-FL-ID.
     MOVE     OUT-ST         TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     STOP     RUN.
 END DECLARATIVES.
****************************************************************
*               コントロール
****************************************************************
 CONTROL-SEC         SECTION.
     DISPLAY  "***   ZMO0120B START  ***"   UPON  CONS.
*
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC    UNTIL  MAIN-FLG  =  99.
     PERFORM  END-SEC.
*
     DISPLAY  "***   ZMO0120B  END   ***"   UPON  CONS.
     STOP  RUN.
 CONTROL-EXIT.
     EXIT.
****************************************************************
*      １．０　　初期処理                                      *
****************************************************************
 INIT-SEC               SECTION.
*ファイル ＯＰＥＮ
     OPEN     I-O       DSPF.
     OPEN     INPUT     HDENJNL.
     OPEN     OUTPUT    OUTFILE.
*システム日付取得
     ACCEPT  WK-SYSDT        FROM  DATE.
*
     INITIALIZE    FLAGS.
     INITIALIZE    COUNTERS.
*
     MOVE    SPACE           TO   DSP-PRO.
     MOVE    "ZMO0120"       TO   DSP-FMT.
     MOVE    SPACE           TO   DSP-ZMO0120.
*
     MOVE    1               TO   MAIN-FLG.
*
 INIT-RTN-EXIT.
     EXIT.
****************************************************************
*      ２．０　　メイン処理                                    *
****************************************************************
 MAIN-SEC                    SECTION.
     EVALUATE      MAIN-FLG
         WHEN      1         PERFORM   BODY-SUB
         WHEN      2         PERFORM   KAKUNIN-SUB
         WHEN      3         PERFORM   FUPDT-SUB
         WHEN      OTHER     CONTINUE
     END-EVALUATE.
 MAIN-END.
     EXIT.
*--------------------------------------------------------------*
*       2.1    範囲指定                                        *
*--------------------------------------------------------------*
 BODY-SUB          SECTION.
*画面表示
     MOVE     1              TO   PFK-FLG.
     PERFORM       DSP-WT-SEC.
*画面入力
     PERFORM       CLR-BODY-RTN.
     MOVE     "GPBODY"       TO   DSP-GRP.
     PERFORM       DSP-RD-SEC.
*
     EVALUATE DSP-FNC
         WHEN "F005"
              MOVE      99        TO   MAIN-FLG
              MOVE      4050      TO   PROGRAM-STATUS
         WHEN "E000"
              PERFORM        BODY-CHK-SUB
              IF  ERR-MSG-CD   =    ZERO
                  MOVE  2         TO   MAIN-FLG
              END-IF
         WHEN OTHER
              MOVE      1         TO   ERR-MSG-CD
     END-EVALUATE.
*
 BODY-SUB-EXIT.
     EXIT.
*--------------------------------------------------------------*
*      2.1.1     範囲指定の入力チェック                        *
*--------------------------------------------------------------*
 BODY-CHK-SUB           SECTION.
*
     IF       DSP-STRICD     IS   NOT  NUMERIC
              MOVE      ZERO      TO   DSP-STRICD
     END-IF.
     IF       DSP-ETRICD     IS   NOT  NUMERIC
              MOVE      ALL "9"   TO   DSP-ETRICD
     END-IF.
     IF       DSP-STRICD     >    DSP-ETRICD
              MOVE      2    TO   ERR-MSG-CD
              MOVE  "R"      TO   EDIT-OPTION    OF   DSP-STRICD
              MOVE  "R"      TO   EDIT-OPTION    OF   DSP-ETRICD
     END-IF.
*納入日
     IF       DSP-YY         IS   NOT  NUMERIC
              IF  ERR-MSG-CD   =    ZERO
                  MOVE   3   TO   ERR-MSG-CD
              END-IF
              MOVE  "R"      TO   EDIT-OPTION    OF   DSP-YY
              MOVE  "C"      TO   EDIT-CURSOR    OF   DSP-YY
     END-IF.
     IF       DSP-MM         IS   NOT  NUMERIC
              IF  ERR-MSG-CD   =    ZERO
                  MOVE   3   TO   ERR-MSG-CD
              END-IF
              MOVE  "R"      TO   EDIT-OPTION    OF   DSP-MM
              MOVE  "C"      TO   EDIT-CURSOR    OF   DSP-MM
     ELSE
              IF ( DSP-MM    =    ZERO ) OR
                 ( DSP-MM    >    12 )
                   IF  ERR-MSG-CD   =    ZERO
                       MOVE   3   TO   ERR-MSG-CD
                   END-IF
                   MOVE  "R" TO   EDIT-OPTION    OF   DSP-MM
                   MOVE  "C" TO   EDIT-CURSOR    OF   DSP-MM
              END-IF
     END-IF.
     IF       DSP-DD         IS   NOT  NUMERIC
              IF  ERR-MSG-CD   =    ZERO
                  MOVE   3   TO   ERR-MSG-CD
              END-IF
              MOVE  "R"      TO   EDIT-OPTION    OF   DSP-DD
              MOVE  "C"      TO   EDIT-CURSOR    OF   DSP-DD
     ELSE
              IF   DSP-DD      =  ZERO
                   IF   ERR-MSG-CD   =    ZERO
                        MOVE   3   TO   ERR-MSG-CD
                   END-IF
                   MOVE  "R" TO   EDIT-OPTION    OF   DSP-DD
                   MOVE  "C" TO   EDIT-CURSOR    OF   DSP-DD
              END-IF
     END-IF.
     IF       ERR-MSG-CD     =    3
              GO     TO   BODY-CHK-END
     END-IF.
*閏年ＣＨＫ
     IF       DSP-YY    >   90
              COMPUTE   WK-NOUYY  =    DSP-YY    +    1900
     ELSE
              COMPUTE   WK-NOUYY  =    DSP-YY    +    2000
     END-IF.
     IF       DSP-MM    =   2
              DIVIDE    WK-NOUYY  BY      4
              GIVING    WK-SHO    REMAINDER    WK-AMARI
              IF     WK-AMARI  =  ZERO
                     MOVE   29    TO   WK-MATUBI(2)
              ELSE
                     MOVE   28    TO   WK-MATUBI(2)
              END-IF
     END-IF.
*月末ＣＨＫ
     IF     DSP-DD      >    WK-MATUBI(DSP-MM)
            IF  ERR-MSG-CD   =  ZERO
                MOVE   3     TO   ERR-MSG-CD
            END-IF
            MOVE  "R"        TO   EDIT-OPTION    OF   DSP-DD
            MOVE  "C"        TO   EDIT-CURSOR    OF   DSP-DD
     END-IF.
*納入日をワークへ設定
     IF     ERR-MSG-CD   =   ZERO
            MOVE   DSP-MM    TO   WK-NOUMM
            MOVE   DSP-DD    TO   WK-NOUDD
     END-IF.
*
 BODY-CHK-END.
     EXIT.
*--------------------------------------------------------------*
*      2.2       確認入力                                      *
*--------------------------------------------------------------*
 KAKUNIN-SUB            SECTION.
*画面表示
     MOVE     2                   TO   PFK-FLG.
     PERFORM       DSP-WT-SEC.
*画面入力
     MOVE     "GPKAK"             TO   DSP-GRP.
     PERFORM       DSP-RD-SEC.
* アテンション判定
     EVALUATE  DSP-FNC
        WHEN  "F004"
               MOVE   SPACE       TO   DSP-BODYG
               MOVE   1           TO   MAIN-FLG
               MOVE   ZERO        TO   ERR-MSG-CD
        WHEN  "F005"
               MOVE   99          TO   MAIN-FLG
               MOVE   4050        TO   PROGRAM-STATUS
        WHEN  "F006"
               MOVE   1           TO   MAIN-FLG
        WHEN  "E000"
               INITIALIZE    COUNTERS
               MOVE   ZERO        TO   HIT-FLG
               PERFORM  DEN-READ  UNTIL  ( HIT-FLG = 1  )  OR
                                         ( MAIN-FLG = 99 )
               IF    MAIN-FLG     =    99
                     CLOSE   HDENJNL
                     OPEN    INPUT     HDENJNL
                     MOVE  1      TO   MAIN-FLG
                     MOVE  5      TO   ERR-MSG-CD
               ELSE
                     MOVE  3      TO   MAIN-FLG
               END-IF
        WHEN  OTHER
              MOVE   1            TO   ERR-MSG-CD
     END-EVALUATE.
*
 KAKUNIN-END.
     EXIT.
*--------------------------------------------------------------*
*    2.3      抽出処理                                         *
*--------------------------------------------------------------*
 FUPDT-SUB              SECTION.
*対象データＷＲＩＴＥ
     MOVE     DEN-REC        TO   OUT-REC.
     WRITE    OUT-REC.
     ADD      1              TO   OUT-CNT.
*伝票ファイルＲＥＡＤ
     MOVE     ZERO           TO   HIT-FLG.
     PERFORM  DEN-READ  UNTIL   ( HIT-FLG = 1  ) OR
                                ( MAIN-FLG = 99 ).
*
 FUPDT-SUB-END.
     EXIT.
*--------------------------------------------------------------*
*    2.2.1        伝票ファイル　 READ                          *
*--------------------------------------------------------------*
 DEN-READ               SECTION.
*
     MOVE     ZERO           TO   SKIP-FLG.
*
     READ     HDENJNL   AT   END
              MOVE      99        TO   MAIN-FLG
              GO        TO        DEN-READ-END
     END-READ.
*売上データ作成ＦＬＧ
     IF       DEN-F277  =    0
              ADD       1         TO   IN-CNT
     ELSE
              MOVE      99        TO   MAIN-FLG
              GO        TO        DEN-READ-END
     END-IF.
*取引先コード
     IF       DEN-F01   <    DSP-STRICD
              MOVE      1         TO   SKIP-FLG
     END-IF.
     IF       DEN-F01   >    DSP-ETRICD
              MOVE      1         TO   SKIP-FLG
     END-IF.
*納入日
     IF       DEN-F112  >    WK-NOUDT
              MOVE      1         TO   SKIP-FLG
     END-IF.
*
*93.12
     IF       DEN-F03   >    10
              ADD       1         TO   SKIP-FLG
     END-IF.
*
     IF       DEN-F15   =    ZERO
              ADD       1         TO   SKIP-FLG
     END-IF.
*
*
     IF       SKIP-FLG  =    ZERO
              MOVE      1         TO   HIT-FLG
     ELSE
              ADD       1         TO   SKIP-CNT
     END-IF.
 DEN-READ-END.
     EXIT.
*--------------------------------------------------------------*
*      2.1.1     画面表示処理                                  *
*--------------------------------------------------------------*
 DSP-WT-SEC                  SECTION.
*ＰＦキー設定
     MOVE  TBL-PFK(PFK-FLG)      TO  DSP-GIDMSG.
*エラー設定
     IF  ERR-MSG-CD     NOT  =    ZERO
         MOVE    ERR-MSG(ERR-MSG-CD)   TO   DSP-ERRMSG
     END-IF.
*画面表示
     MOVE    "GPALL"         TO   DSP-GRP.
     MOVE     SPACE          TO   DSP-PRO.
     WRITE    DSP-ZMO0120.
*エラークリア
     MOVE  ZERO              TO   ERR-MSG-CD.
     MOVE  SPACE             TO   DSP-ERRMSG.
 DSP-WT-END.
     EXIT.
*--------------------------------------------------------------*
*      2.1.2     画面データの入力処理                          *
*--------------------------------------------------------------*
 DSP-RD-SEC             SECTION.
     MOVE  "NE"         TO   DSP-PRO.
     READ   DSPF.
 DSP-RD-END.
     EXIT.
*--------------------------------------------------------------*
*      2.1.3     ＢＯＤＹ　属性クリア                          *
*--------------------------------------------------------------*
 CLR-BODY-RTN           SECTION.
*
     MOVE    " "        TO   EDIT-CURSOR OF DSP-STRICD
                             EDIT-CURSOR OF DSP-ETRICD
                             EDIT-CURSOR OF DSP-YY
                             EDIT-CURSOR OF DSP-MM
                             EDIT-CURSOR OF DSP-DD.
     MOVE    "M"        TO   EDIT-OPTION OF DSP-STRICD
                             EDIT-OPTION OF DSP-ETRICD
                             EDIT-OPTION OF DSP-YY
                             EDIT-OPTION OF DSP-MM
                             EDIT-OPTION OF DSP-DD.
*
 CLR-BODY-EXIT.
     EXIT.
****************************************************************
*      3.0        終了処理                                     *
****************************************************************
 END-SEC                SECTION.
     CLOSE    DSPF.
     CLOSE    HDENJNL.
     CLOSE    OUTFILE.
*
     DISPLAY  "***   HDENJNL    IN = "  IN-CNT    UPON CONS.
     DISPLAY  "***   HDENJNL  SKIP = "  SKIP-CNT  UPON CONS.
     DISPLAY  "***   HDENJNL   OUT = "  OUT-CNT   UPON CONS.
*
 END-SEC-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
