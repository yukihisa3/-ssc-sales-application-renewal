****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　出荷　　　　　　　　　　          *
*    モジュール名　　　　：　出荷検品結果集計　　　　　　　    *
*                              （カインズＴＣ１）              *
*    作成日／更新日　　　：　2019/06/25                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　検品結果データの集計　　　　　　　*
*                        ：　（バッチ・倉庫・納品日・伝票番号) *
*                        ：　紐付情報の保管を行なう。　　　　　*
*    作成日／更新日　　　：　2024/06/21                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　梱包ＮＯが入っていない場合は、　　*
*                        ：　対象としない様に変更　　　　　　　*
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            NKE1040B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2019/06/25.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       PG6000.
 OBJECT-COMPUTER.       PG6000.
 SPECIAL-NAMES.
     CONSOLE       IS        CONS
     STATION       IS        STAT.
****************************************************************
 INPUT-OUTPUT              SECTION.
****************************************************************
 FILE-CONTROL.
*----<<カインズ検品結果データ>>----*
     SELECT   RCVCZDXX  ASSIGN              DA-01-S-RCVCZDXX
                        ORGANIZATION        SEQUENTIAL
                        FILE      STATUS    IS   RCV-ST.
*----<<商品変換テーブル>>----*
     SELECT   SHOTBL1   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TBL-F01   TBL-F02
                        FILE      STATUS    IS   TBL-ST.
*----<<検品結果集計ファイル>>----*
     SELECT   CZSUMXX   ASSIGN              DA-01-VS-CZSUMXX
                        ORGANIZATION        SEQUENTIAL
                        FILE      STATUS    IS   SUM-ST.
*----<<検品紐付情報ファイル>>----*
     SELECT   CZKONXX   ASSIGN              DA-01-VS-CZKONXX
                        ORGANIZATION        SEQUENTIAL
                        FILE      STATUS    IS   KON-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<<カインズ検品結果データ>>----*
 FD  RCVCZDXX           LABEL RECORD   IS   STANDARD
                        BLOCK CONTAINS 31 RECORDS.
     COPY     RCVCZDXX  OF        XFDLIB
              JOINING   RCV  AS   PREFIX.
*----<<商品変換テーブル>>----*
 FD  SHOTBL1            LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TBL  AS   PREFIX.
*----<<検品結果集計ファイル>>----*
 FD  CZSUMXX            LABEL RECORD   IS   STANDARD.
     COPY     CZSUMF    OF        XFDLIB
              JOINING   SUM  AS   PREFIX.
*----<<検品紐付情報ファイル>>----*
 FD  CZKONXX            LABEL RECORD   IS   STANDARD.
     COPY     CZKONXX   OF        XFDLIB
              JOINING   KON  AS   PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  RCV-END        PIC  X(03)   VALUE SPACE.
     03  TBL-INV        PIC  X(03)   VALUE SPACE.
 01  WK-CNT.
     03  RCV-CNT        PIC  9(07).
     03  SUM-CNT        PIC  9(07).
     03  KON-CNT        PIC  9(07).
     03  CAL-F16        PIC  9(07).
*#2024/06/21 NAV ST
     03  KOP-CNT       PIC  9(07).
*#2024/06/21 NAV ED
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
     03  RCV-ST         PIC  X(02).
     03  TBL-ST         PIC  X(02).
     03  SUM-ST         PIC  X(02).
     03  KON-ST         PIC  X(02).
*----<< ﾌﾞﾚｲｸKEY   >>--*
     03  BRK-F01        PIC  9(08).
     03  BRK-F02        PIC  9(04).
     03  BRK-F03        PIC  9(08).
     03  BRK-F04        PIC  X(02).
     03  BRK-F05        PIC  9(08).
     03  BRK-F06        PIC  9(05).
     03  BRK-F07        PIC  9(08).
     03  BRK-F08        PIC  9(05).
     03  BRK-F10        PIC  9(09).
     03  BRK-F11        PIC  9(02).
*----<< ﾌﾞﾚｲｸKEY保管   >>--*
     COPY     RCVCZDXX  OF        XFDLIB
              JOINING   BRK-RCV   AS   PREFIX.
*
 01  PG-ID             PIC  X(08)      VALUE  "NKE1040B".
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-YYMD           PIC  9(08).
 01  FILLER             REDEFINES      SYS-YYMD.
     03  SYS-YYYY       PIC  9(04).
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC X(01).
 01  LINK-IN-YMD6            PIC 9(06).
 01  LINK-IN-YMD8            PIC 9(08).
 01  LINK-OUT-RET            PIC X(01).
 01  LINK-OUT-YMD            PIC 9(08).
*リンケージ
 LINKAGE                SECTION.
 01  PARA-IN-BUMON           PIC X(04).
 01  PARA-IN-TANTOU          PIC X(02).
 01  PARA-IN-SOUKO           PIC X(02).
 01  PARA-IN-TDATE           PIC 9(08).
 01  PARA-IN-TTIME           PIC 9(06).
****************************************************************
 PROCEDURE              DIVISION  USING  PARA-IN-BUMON
                                         PARA-IN-TANTOU
                                         PARA-IN-SOUKO
                                         PARA-IN-TDATE
                                         PARA-IN-TTIME.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
 RCVCZDXX-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      RCVCZDXX.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### NKE1040B RCVCZDXX ERROR " RCV-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 SHOTBL1-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SHOTBL1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### NKE1040B SHOTBL1  ERROR " TBL-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 CZSUMXX-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      CZSUMXX.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### NKE1040B CZSUMXX  ERROR " SUM-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 CZKONXX-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      CZKONXX.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### NKE1040B CZKONXX  ERROR " KON-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN  UNTIL  RCV-END  =  "END".
     PERFORM  300-END-RTN.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** NKE1040B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*ファイルＯＰＥＮ
     OPEN     INPUT     RCVCZDXX
                        SHOTBL1.
     OPEN     OUTPUT    CZSUMXX
                        CZKONXX.
*クリア
     INITIALIZE    WK-CNT  FLAGS.
*
*カインズ検品結果データ初期ＲＥＡＤ
     PERFORM       RCV-RD-SEC.
     IF            RCV-END  =  "END"
          DISPLAY  NC"＃対象データなし！！＃＃" UPON CONS
     END-IF.
     MOVE          RCV-F01      TO    BRK-F01.
     MOVE          RCV-F02      TO    BRK-F02.
     MOVE          RCV-F03      TO    BRK-F03.
     MOVE          RCV-F04      TO    BRK-F04.
     MOVE          RCV-F05      TO    BRK-F05.
     MOVE          RCV-F06      TO    BRK-F06.
     MOVE          RCV-F07      TO    BRK-F07.
     MOVE          RCV-F08      TO    BRK-F08.
     MOVE          RCV-F10      TO    BRK-F10.
     MOVE          RCV-F11      TO    BRK-F11.
     MOVE          RCV-REC      TO    BRK-RCV-REC.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*
 200-MAIN-01.
*集計単位ブレイク判定
*    非ブレイク
     IF ( RCV-F01  =  BRK-F01 ) AND
        ( RCV-F02  =  BRK-F02 ) AND
        ( RCV-F03  =  BRK-F03 ) AND
        ( RCV-F04  =  BRK-F04 ) AND
        ( RCV-F05  =  BRK-F05 ) AND
        ( RCV-F06  =  BRK-F06 ) AND
        ( RCV-F07  =  BRK-F07 ) AND
        ( RCV-F08  =  BRK-F08 ) AND
        ( RCV-F10  =  BRK-F10 ) AND
        ( RCV-F11  =  BRK-F11 )
          ADD      RCV-F16      TO    CAL-F16
          PERFORM  KON-OUT-SEC
*    ブレイク
     ELSE
          PERFORM  SUM-OUT-SEC
          MOVE     ZERO         TO    CAL-F16
          MOVE     RCV-F01      TO    BRK-F01
          MOVE     RCV-F02      TO    BRK-F02
          MOVE     RCV-F03      TO    BRK-F03
          MOVE     RCV-F04      TO    BRK-F04
          MOVE     RCV-F05      TO    BRK-F05
          MOVE     RCV-F06      TO    BRK-F06
          MOVE     RCV-F07      TO    BRK-F07
          MOVE     RCV-F08      TO    BRK-F08
          MOVE     RCV-F10      TO    BRK-F10
          MOVE     RCV-F11      TO    BRK-F11
          MOVE     RCV-REC      TO    BRK-RCV-REC
          ADD      RCV-F16      TO    CAL-F16
          PERFORM  KON-OUT-SEC
     END-IF.
*
*カインズ検品結果データＲＥＡＤ
 200-MAIN-02.
     PERFORM RCV-RD-SEC.
*ＡＴ　ＥＮＤ　→　集計ファイル最終レコード出力
 200-MAIN-03.
     IF   RCV-END  =  "END"
          IF  RCV-CNT  NOT = ZERO
              PERFORM  SUM-OUT-SEC
              GO       TO  200-MAIN-RTN-EXIT
          END-IF
     END-IF.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
*ファイルＣＬＯＳＥ
     CLOSE    RCVCZDXX
              SHOTBL1
              CZSUMXX
              CZKONXX.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  NC"検品結果　読込＝" RCV-CNT UPON CONS.
     DISPLAY  NC"結果集計　出力＝" SUM-CNT UPON CONS.
     DISPLAY  NC"紐付情報　出力＝" KON-CNT UPON CONS.
*#2024/06/21 NAV ST
     DISPLAY  NC"梱包_ＮＧ件数＝" KOP-CNT UPON CONS.
*#2024/06/21 NAV ED
     DISPLAY  "*** NKE1040B END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*    IF   RCV-CNT  =  ZERO
*         MOVE     "4010"      TO      PROGRAM-STATUS
*    END-IF.
*
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*       カインズ検品結果データ読込　                           *
*--------------------------------------------------------------*
 RCV-RD-SEC             SECTION.
     READ   RCVCZDXX
        AT  END
            MOVE  "END"  TO  RCV-END
*#2024/06/21 NAV ST
            GO           TO  RCV-RD-EXIT
*#2024/06/21 NAV ED
        NOT AT  END
            ADD    1     TO  RCV-CNT
     END-READ.
*#2024/06/21 NAV ST
*梱包ＮＯが空白の場合はデータ対象としない
     IF     RCV-F09  =  SPACE
            ADD    1     TO  KOP-CNT
            GO           TO  RCV-RD-SEC
     END-IF.
*#2024/06/21 NAV ED
*
 RCV-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*       検品紐付情報ファイル出力　　                           *
*--------------------------------------------------------------*
 KON-OUT-SEC            SECTION.
*
     MOVE    SPACE          TO        KON-REC.
     INITIALIZE                       KON-REC.
*バッチ日付
     MOVE    RCV-F01        TO        KON-F01.
*バッチ時刻
     MOVE    RCV-F02        TO        KON-F02.
*取引先ＣＤ
     MOVE    RCV-F03        TO        KON-F03.
*倉庫ＣＤ
     MOVE    RCV-F04        TO        KON-F04.
*センター納品日
     MOVE    RCV-F05        TO        KON-F05.
*センターＣＤ
     MOVE    RCV-F06        TO        KON-F06.
*店舗納品日
     MOVE    RCV-F07        TO        KON-F07.
*店舗ＣＤ
     MOVE    RCV-F08        TO        KON-F08.
*梱包_
     MOVE    ALL "0"        TO        KON-F09.
     MOVE    RCV-F09        TO        KON-F09.
*伝票番号
     MOVE    RCV-F10        TO        KON-F10.
*行番号
     MOVE    RCV-F11        TO        KON-F11.
*サカタ商品ＣＤ＋品単ＣＤ　＆　棚番
     MOVE    RCV-F12        TO        KON-F12.
*    商品変換テーブル検索
     MOVE    RCV-F03        TO        TBL-F01.
     MOVE    RCV-F13        TO        TBL-F02.
     READ    SHOTBL1
       INVALID
             CONTINUE
       NOT  INVALID
*            棚番セット
             MOVE TBL-F08   TO        KON-F14
*            サカタ商品ＣＤ＋品単ＣＤ再セット
             MOVE TBL-F03   TO        KON-F12
     END-READ.
*ＪＡＮＣＤ
     MOVE    RCV-F13        TO        KON-F13.
*発注数
     MOVE    RCV-F15        TO        KON-F15.
*検品数
     MOVE    RCV-F16        TO        KON-F16.
*出荷総梱包数 F17
*    後方ＰＧにてセット
*取込日付
     MOVE    PARA-IN-TDATE  TO        KON-F94.
*取込時刻
     MOVE    PARA-IN-TTIME  TO        KON-F95.
*取込担当者部門
     MOVE    PARA-IN-BUMON  TO        KON-F96.
*取込担当者ＣＤ
     MOVE    PARA-IN-TANTOU TO        KON-F97.
*レコード出力
     WRITE   KON-REC.
     ADD     1              TO        KON-CNT.
*
 KON-OUT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*       検品結果集計ファイル出力　　                           *
*--------------------------------------------------------------*
 SUM-OUT-SEC            SECTION.
*
     MOVE    SPACE             TO        SUM-REC.
     INITIALIZE                          SUM-REC.
*バッチ日付
     MOVE    BRK-RCV-F01        TO        SUM-F01.
*バッチ時刻
     MOVE    BRK-RCV-F02        TO        SUM-F02.
*取引先ＣＤ
     MOVE    BRK-RCV-F03        TO        SUM-F03.
*倉庫ＣＤ
     MOVE    BRK-RCV-F04        TO        SUM-F04.
*センター納品日
     MOVE    BRK-RCV-F05        TO        SUM-F05.
*センターＣＤ
     MOVE    BRK-RCV-F06        TO        SUM-F06.
*店舗納品日
     MOVE    BRK-RCV-F07        TO        SUM-F07.
*店舗ＣＤ
     MOVE    BRK-RCV-F08        TO        SUM-F08.
*梱包_
     MOVE    ALL "0"           TO        SUM-F09.
*伝票番号
     MOVE    BRK-RCV-F10        TO        SUM-F10.
*行番号
     MOVE    BRK-RCV-F11        TO        SUM-F11.
*サカタ商品ＣＤ＋品単ＣＤ　＆　棚番
     MOVE    BRK-RCV-F12        TO        SUM-F12.
*    商品変換テーブル検索
     MOVE    BRK-RCV-F03        TO        TBL-F01.
     MOVE    BRK-RCV-F13        TO        TBL-F02.
     READ    SHOTBL1
       INVALID
             CONTINUE
       NOT  INVALID
*            棚番セット
             MOVE TBL-F08      TO        SUM-F14
*            サカタ商品ＣＤ＋品単ＣＤ再セット
             MOVE TBL-F03      TO        SUM-F12
     END-READ.
*ＪＡＮＣＤ
     MOVE    BRK-RCV-F13        TO        SUM-F13.
*発注数
     MOVE    BRK-RCV-F15        TO        SUM-F15.
*検品数
     MOVE    CAL-F16           TO        SUM-F16.
*出荷総梱包数 F17
*    後方ＰＧにてセット
*取込日付
     MOVE    PARA-IN-TDATE     TO        SUM-F94.
*取込時刻
     MOVE    PARA-IN-TTIME     TO        SUM-F95.
*取込担当者部門
     MOVE    PARA-IN-BUMON     TO        SUM-F96.
*取込担当者ＣＤ
     MOVE    PARA-IN-TANTOU    TO        SUM-F97.
*レコード出力
     WRITE   SUM-REC.
     ADD     1                 TO        SUM-CNT.
*
 SUM-OUT-EXIT.
     EXIT.
