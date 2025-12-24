****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ　　　　　　　　*
*    業務名　　　　　　　：　ＤＣＭサンワ　　　　　　　　　　　*
*    モジュール名　　　　：　受領書発行　　　　　　　　　　　　*
*    作成日／更新日　　　：　2015/11/17                        *
*    作成者／更新者　　　：　ＮＡＶ宮下　　　　　　　　　　　　*
*    処理概要　　　　　　：　ＤＣＭサンワ受領よりＤＣＭサンワ  *
*                        ：　受領書を出力する　　　　　　　　  *
*    更新日／更新者　　　：　                                  *
*    更新概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION            DIVISION.
****************************************************************
 PROGRAM-ID.               SSY7510L.
 AUTHOR.                   NAV.
 DATE-WRITTEN.             2015/11/17.
****************************************************************
 ENVIRONMENT               DIVISION.
****************************************************************
 CONFIGURATION             SECTION.
 SOURCE-COMPUTER.
 OBJECT-COMPUTER.
 SPECIAL-NAMES.
*    STATION     IS        STA
     YA          IS        NIHONGO
     YB          IS        YB
     YB-21       IS        YB-21
     YB-22       IS        YB-22
     CONSOLE     IS        CONS.
****************************************************************
 INPUT-OUTPUT           SECTION.
****************************************************************
 FILE-CONTROL.
*----<< ＤＣＭサンワ受領データ >>--*
     SELECT     SWJYRWF    ASSIGN    TO        DA-01-VI-SWJYRWL1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      SEQUENTIAL
                           RECORD    KEY       WK-F01
                                               WK-F03
                                               WK-F05
                                               WK-F06
                                               WK-F07
                           FILE      STATUS    WK-ST.
*----<< 帳票ファイル >>--*
     SELECT     PRTFILE    ASSIGN              01-GS-PRTF
                           DESTINATION         "PRT"
                           FORMAT              PRT-FMT
                           GROUP               PRT-GRP
                           PROCESSING          PRT-PRO
                           CONTROL             PRT-CTL
                           STATUS              PRT-ST
                                               PRT-ST2.
****************************************************************
 DATA                      DIVISION.
****************************************************************
 FILE                      SECTION.
*----<< ＤＣＭサンワ受領データ >>--*
 FD  SWJYRWF     LABEL       RECORD    IS        STANDARD.
     COPY        SWJYRWF   OF        XFDLIB
     JOINING     WK        AS        PREFIX.
*
*----<< 帳票ファイル >>--*
 FD  PRTFILE.
     COPY        FSY75102  OF        XMDLIB
     JOINING     PRT       AS        PREFIX.
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
*
*----<< プログラム名 >>--*
 01  PG-ID                  PIC  X(08)  VALUE "SSY7510L".
*
*----<< 帳票パラメータ >>--*
 01  PRT-AREA.
     03  PRT-FMT            PIC  X(08).
     03  PRT-PRO            PIC  X(02).
     03  PRT-GRP            PIC  X(08).
     03  PRT-CTL.
         05  PRT-CNTRL      PIC  X(04).
         05  PRT-STR-PG     PIC  X(02).
     03  PRT-ST             PIC  X(02).
     03  PRT-ST2            PIC  X(04).
*
*----<< 固定項目 >>--*
 01  MAX-LINE                PIC  9(02)  VALUE   50.
*
*----<< ファイルステータス >>--*
 01  FILE-STATUS.
     03  WK-ST               PIC  X(02).
*
*----<< 変数 >>--*
 01  WK-AREA.
     03  END-SW              PIC  9(01)  VALUE   ZERO.
     03  ERR-SW              PIC  9(01)  VALUE   ZERO.
     03  PAGE-CNT            PIC  9(05)  VALUE   ZERO.
     03  LINE-CNT            PIC  9(05)  VALUE   ZERO.
*
*----<< 集計変数 >>--*
 01  DKEI-AREA.
     03  DHACSU              PIC  9(07).
     03  DKESSU              PIC  9(07).
     03  DSAISU              PIC  9(07).
     03  DGENKA              PIC  9(08).
*
 01  TKEI-AREA.
     03  THACSU              PIC  9(07).
     03  TKESSU              PIC  9(07).
     03  TSAISU              PIC  9(07).
     03  TGENKA              PIC  9(08).
*
 01  SKEI-AREA.
     03  SHACSU              PIC  9(07).
     03  SKESSU              PIC  9(07).
     03  SSAISU              PIC  9(07).
     03  SGENKA              PIC  9(08).
*
*----<< ブレイク項目 >>--*
 01  NEW-KEY.
     02  NEW-TENCD           PIC  9(05).
     02  NEW-DENNO           PIC  9(08).
*
 01  OLD-KEY.
     02  OLD-TENCD           PIC  9(05).
     02  OLD-DENNO           PIC  9(08).
*
*----<< 検収日変換 >>--*
 01  KENSYUBI.
     03  KENSYUBI-NEN        PIC  9(04).
     03  FILLER              PIC  X(01)  VALUE  "/".
     03  KENSYUBI-TSUKI      PIC  9(02).
     03  FILLER              PIC  X(01)  VALUE  "/".
     03  KENSYUBI-HI         PIC  9(02).
*
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME             PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-YS               PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y            PIC  9(02)  VALUE  ZERO.
         05  WK-M            PIC  9(02)  VALUE  ZERO.
         05  WK-D            PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE            PIC  9(08).
*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC  X(01).
 01  LINK-IN-YMD6            PIC  9(06).
 01  LINK-IN-YMD8            PIC  9(08).
 01  LINK-OUT-RET            PIC  X(01).
 01  LINK-OUT-YMD            PIC  9(08).
*
*----<< 店舗コード変換 >>--*
 01  TEMPOCD-MOJI           PIC   X(05).
 01  FILLER            REDEFINES      TEMPOCD-MOJI.
     03  TEMPOCD-SUCHI      PIC  9(05).
*
*----<< 伝票番号変換 >>--*
 01  DEMPYONO-MOJI          PIC   X(10).
 01  FILLER            REDEFINES      DEMPYONO-MOJI.
     03  DEMPYONO-SUCHI     PIC   9(10).
*
*----<< 行番号変換 >>--*
 01  GYONO-MOJI             PIC   X(04).
 01  FILLER            REDEFINES      GYONO-MOJI.
     03  GYONO-SUCHI        PIC   9(04).
*
 01  IN-DATA                 PIC  X(01).
 01  FILE-ERR.
     03  WK-ERR              PIC  N(10) VALUE
         NC"受領書ワークエラー".
     03  PRT-ERR             PIC  N(10) VALUE
         NC"プリンターエラー".
*
*----<< ＭＳＧエリア >>--*
 01  MSG-AREA.
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
*
 01  IX                      PIC  9(04) VALUE  0.
 01  READ-CNT                PIC  9(07) VALUE  0.
 01  IN-CNT                  PIC  9(07) VALUE  0.
 01  SET-FLG                 PIC  X(03) VALUE  SPACE.
 01  HENKAN-FLG              PIC  X(03) VALUE  SPACE.
 01  DEN-FLG                 PIC  9(01) VALUE  0.
 01  HIT-FLG1                PIC  9(01) VALUE  0.
 01  HIT-FLG2                PIC  9(01) VALUE  0.
 01  WK-GENKA                PIC S9(08) VALUE  0.
*
 01  HEN-GNOX.
     02  HEN-GNO             PIC  9(02).
     02  FILLER              PIC  X(02).
*
 01  HEN-DENNOX.
     02  HEN-DENNO           PIC  9(07).
     02  FILLER              PIC  X(03).
****************************************************************
 PROCEDURE                 DIVISION.
****************************************************************
 DECLARATIVES.
 PRT-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE PRTFILE.
     DISPLAY     PRT-ERR   UPON      CONS.
     DISPLAY     PRT-ST    UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
*     ACCEPT      IN-DATA   FROM      CONS.
     STOP        RUN.
 WK-ERR                    SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SWJYRWF.
     DISPLAY     WK-ERR    UPON      CONS.
     DISPLAY     WK-ST     UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
*     ACCEPT      IN-DATA   FROM      CONS.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 END DECLARATIVES.
***********************************************************
*                     ＭＡＩＮ処理
***********************************************************
 PROC-SEC                  SECTION.
*
 PROC-010.
     PERFORM     INIT-SEC.
*
     PERFORM     MAIN-SEC
                 UNTIL END-SW = 1.
*
     PERFORM     END-SEC.
*
     STOP        RUN.
 PROC-EXIT.
     EXIT.
**********************************************************
*                      Ｉ Ｎ Ｉ Ｔ
**********************************************************
 INIT-SEC                  SECTION.
*
*----<< 処理名セット >>--*
     MOVE    "INIT-SEC"           TO    S-NAME.
*
*----<< 開始メッセージ出力 >>--*
     CALL    "SKYMSGS" USING     PG-ID.
*
*----<< 改ページ制御 >--*
     MOVE      99                 TO   LINE-CNT.
*
*----<< 変数クリア >--*
     MOVE      ZERO               TO   PAGE-CNT.
*
*----<< システム日付取得 >>--*
     ACCEPT   WK-DATE           FROM   DATE.
     MOVE      20                 TO   WK-YS.
     INITIALIZE                        DKEI-AREA
                                       TKEI-AREA
                                       SKEI-AREA.
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
*
*----<< システム時刻取得 >>--*
     ACCEPT    WK-TIME          FROM   TIME.
*
*----<<FILE OPEN >>--*
     OPEN        INPUT     SWJYRWF
                 OUTPUT    PRTFILE.
*
*----<< １レコード目読込 >>--*
     PERFORM     WK-READ-SEC.
*
*----<< ブレイクしないように制御 >>--*
     MOVE     WK-F03             TO   OLD-TENCD.
     MOVE     WK-F06             TO   OLD-DENNO.
*
 INIT-EXIT.
     EXIT.
***********************************************************
*                     ＭＡＩＮ処理
***********************************************************
 MAIN-SEC                  SECTION.
*
*----<< ブレイク判定 >>--*
     IF    NEW-KEY    NOT =  OLD-KEY
       IF  NEW-TENCD  NOT =  OLD-TENCD
           PERFORM     DENWRT-SEC
           PERFORM     TENWRT-SEC
           MOVE    0   TO        HIT-FLG1  HIT-FLG2
       ELSE
           PERFORM     DENWRT-SEC
           MOVE    0   TO        HIT-FLG2
       END-IF
     END-IF.
*
*----<< 集計処理 >>--*
     PERFORM     SYUKEI-SEC.
*
*----<< 明細出力 >>--*
     PERFORM     MEIWRT-SEC.
*
*----<< 次レコード読込 >>--*
     PERFORM     WK-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
**********************************************************
*                       Ｅ Ｎ Ｄ
**********************************************************
 END-SEC                   SECTION.
*
*----<< 合計欄出力 >>--*
     IF    IN-CNT      >   ZERO
           PERFORM     DENWRT-SEC
           PERFORM     TENWRT-SEC
     END-IF.
*
*----<<FILE CLOSE >>--*
     CLOSE       SWJYRWF.
     CLOSE       PRTFILE.
*
*----<< 終了メッセージ >>--*
     CALL    "SKYMSGE" USING     PG-ID.
*
 END-EXIT.
     EXIT.
**********************************************************
*                    集計処理
**********************************************************
 SYUKEI-SEC                   SECTION.
*----<< 原価金額を計算 >>--*
     MOVE      ZERO               TO        WK-GENKA.
     COMPUTE  WK-GENKA  =  WK-GENKA  + ( WK-F12 * WK-F14 ).
*----<< 伝票計 >>--*
     ADD       WK-F11             TO        DHACSU.
     ADD       WK-F12             TO        DKESSU.
     ADD       WK-F13             TO        DSAISU.
     ADD       WK-GENKA           TO        DGENKA.
*
*----<< 店舗計 >>--*
     ADD       WK-F11             TO        THACSU.
     ADD       WK-F12             TO        TKESSU.
     ADD       WK-F13             TO        TSAISU.
     ADD       WK-GENKA           TO        TGENKA.
*
 SYUKEI-EXIT.
     EXIT.
**********************************************************
*                 見出しデータ編集書き出し
**********************************************************
 HEAD-WRT-SEC                  SECTION.
*
*----<< クリア >>--*
     MOVE      SPACE              TO        PRT-HEAD1X.
*
*----<< システム日付時刻 >>--*
     MOVE      SYS-DATE(1:4)      TO        PRT-SYSYY.
     MOVE      WK-M               TO        PRT-SYSMM.
     MOVE      WK-D               TO        PRT-SYSDD.
     MOVE      WK-TIME(1:2)       TO        PRT-SYSHH.
     MOVE      WK-TIME(3:2)       TO        PRT-SYSMN.
     MOVE      WK-TIME(5:2)       TO        PRT-SYSSS.
*----<< 頁セット >>--*
     ADD       1                  TO        PAGE-CNT.
     MOVE      PAGE-CNT           TO        PRT-LPAGE.
*----<< 項目セット >>--*
     IF  WK-F96  =  "1"
         MOVE      NC"受信日"     TO        PRT-HINM
         MOVE      WK-F97         TO        PRT-DATEF
     ELSE
         MOVE      NC"検収日"     TO        PRT-HINM
         MOVE      WK-F98         TO        PRT-DATEF
         MOVE      NC"～"         TO        PRT-KARA1
         MOVE      WK-F99         TO        PRT-DATET
     END-IF.
     MOVE      WK-F01             TO        PRT-TORICD.
     MOVE      WK-F02             TO        PRT-TORINM.
*
*****IF  PAGE-CNT  >  ZERO
         MOVE      "HEAD1"        TO        PRT-GRP.
         MOVE      "FSY75102"     TO        PRT-FMT.
         MOVE      "PW"           TO        PRT-PRO.
         MOVE      "A001"         TO        PRT-CTL.
         WRITE     PRT-FSY75102.
*****END-IF.
*
     MOVE      ZERO               TO        LINE-CNT.
*
     MOVE      10                 TO        LINE-CNT.
     MOVE      0                  TO        HIT-FLG1  HIT-FLG2.
*
 HEAD-WRT-EXIT.
     EXIT.
**********************************************************
*                    明細データ書き出し
**********************************************************
 MEIWRT-SEC                  SECTION.
*
*----<< 改ページチェック >>--*
     IF  LINE-CNT       >         MAX-LINE
         PERFORM    HEAD-WRT-SEC
     END-IF.
*
*----<< クリア >>--*
     MOVE      SPACE              TO        PRT-MEIS1X.
     MOVE      SPACE              TO        PRT-MEIS2X.
*
*----<< 店舗コードチェック >>--*
     IF  HIT-FLG1  =  0
         MOVE  WK-F03             TO        PRT-TENCD
         MOVE  WK-F04             TO        PRT-TENNM
         MOVE  "MEIS1"            TO        PRT-GRP
         MOVE  "FSY75102"         TO        PRT-FMT
         MOVE  "PW"               TO        PRT-PRO
         MOVE  "A001"             TO        PRT-CTL
         WRITE PRT-FSY75102
*
         ADD   1                  TO        LINE-CNT
         MOVE  NEW-TENCD          TO        OLD-TENCD
         MOVE  1                  TO        HIT-FLG1
     END-IF.
*
*----<< 伝票番号チェック >>--*
     IF  HIT-FLG2  =  0
         MOVE  WK-F05(1:4)        TO        KENSYUBI-NEN
         MOVE  WK-F05(5:2)        TO        KENSYUBI-TSUKI
         MOVE  WK-F05(7:2)        TO        KENSYUBI-HI
         MOVE  KENSYUBI           TO        PRT-KENSBI
         MOVE  WK-F06             TO        PRT-NDENNO
         MOVE  NEW-DENNO          TO        OLD-DENNO
         MOVE  1                  TO        HIT-FLG2
     END-IF.
*
*----<< 項目セット >>--*
     MOVE      WK-F07             TO        PRT-NGYONO.
     MOVE      WK-F08             TO        PRT-SHOHCD.
     MOVE      WK-F09             TO        PRT-SHOHNM(1:20).
     MOVE      WK-F10             TO        PRT-SHOHNM(21:15).
     MOVE      WK-F11             TO        PRT-HACSU.
     MOVE      WK-F12             TO        PRT-KESSU.
     MOVE      WK-F13             TO        PRT-SAISU.
     MOVE      WK-F14             TO        PRT-GETAN.
*****MOVE      WK-F15             TO        PRT-BATAN.
*****COMPUTE  WK-GENKA  =  WK-GENKA  + ( WK-F12 * WK-F14 ).
     MOVE      WK-GENKA           TO        PRT-BAIKK.
     MOVE      WK-F16             TO        PRT-HDENNO.
     MOVE      WK-F17             TO        PRT-HGYONO.
*
*----<< 改ページチェック >>--*
*****IF  LINE-CNT       >         MAX-LINE
*        PERFORM    HEAD-WRT-SEC
*****END-IF.
*
*----<< 明細出力 >>--*
     MOVE  "MEIS2"                TO        PRT-GRP.
     MOVE  "FSY75102"             TO        PRT-FMT.
     MOVE  "PW"                   TO        PRT-PRO.
     MOVE  "A001"                 TO        PRT-CTL.
     WRITE PRT-FSY75102.
*
     ADD   1                      TO        LINE-CNT.
*
 MEIWRT-EXIT.
     EXIT.
**********************************************************
*                    伝票合計書き出し
**********************************************************
 DENWRT-SEC                   SECTION.
*
*----<< 改ページチェック >>--*
     IF  LINE-CNT       >         MAX-LINE
         PERFORM    HEAD-WRT-SEC
     END-IF.
*
*----<< 項目セット >>--*
     MOVE      DHACSU             TO        PRT-DHASU.
     MOVE      DKESSU             TO        PRT-DKESU.
     MOVE      DSAISU             TO        PRT-DSASU.
     MOVE      DGENKA             TO        PRT-DGKK.
*
*----<< 出力 >>--*
     MOVE  "DENKEI"               TO        PRT-GRP.
     MOVE  "FSY75102"             TO        PRT-FMT.
     MOVE  "PW"                   TO        PRT-PRO.
     MOVE  "A001"                 TO        PRT-CTL.
     WRITE PRT-FSY75102.
*
     ADD   2                      TO        LINE-CNT.
*
*----<< 集計変数クリア >>--*
     INITIALIZE                             DKEI-AREA.
*
 DENWRT-EXIT.
     EXIT.
**********************************************************
*                    店舗合計書き出し
**********************************************************
 TENWRT-SEC                   SECTION.
*
*----<< 改ページチェック >>--*
     IF  LINE-CNT       >         MAX-LINE
         PERFORM    HEAD-WRT-SEC
     END-IF.
*
*----<< 項目セット >>--*
     MOVE      THACSU             TO        PRT-THASU.
     MOVE      TKESSU             TO        PRT-TKESU.
     MOVE      TSAISU             TO        PRT-TSASU.
     MOVE      TGENKA             TO        PRT-TGKK.
*
*----<< 出力 >>--*
     MOVE  "TENKEI"               TO        PRT-GRP.
     MOVE  "FSY75102"             TO        PRT-FMT.
     MOVE  "PW"                   TO        PRT-PRO.
     MOVE  "A001"                 TO        PRT-CTL.
     WRITE PRT-FSY75102.
*
     ADD   2                      TO        LINE-CNT.
*
*----<< 集計変数クリア >>--*
     INITIALIZE                             TKEI-AREA.
*
 TENWRT-EXIT.
     EXIT.
****************************************************************
*             ワーク読込み
****************************************************************
 WK-READ-SEC             SECTION.
*
*----<< ワーク読込 >>--*
     READ     SWJYRWF  NEXT  AT  END
              MOVE     1         TO   END-SW
              MOVE     99999999  TO   NEW-TENCD
              MOVE     9999999   TO   NEW-DENNO
              GO                 TO   WK-READ-EXIT
     END-READ.
*
*----<< ブレイク項目セット >>--*
     MOVE     WK-F03             TO   NEW-TENCD.
     MOVE     WK-F06             TO   NEW-DENNO.
*
*----<< 読込件数カウントアップ >>--*
     ADD      1                  TO   IN-CNT.
*
 WK-READ-EXIT.
     EXIT.
